// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "lib/forge-std/src/Test.sol";
import {FullMath} from "uni-v3-lib/FullMath.sol";
import {RationalToQ128x128} from "./RationalToQ128x128.sol";

/// @title MuldivTest
/// @notice Tests RationalToQ128x128.mulDiv against the known-good implementation in FullMath
contract MuldivTest is Test {
    // Expose wrappers so we can use try/catch (requires external call boundary)
    function fullMathMulDiv(uint256 a, uint256 b, uint256 d) public pure returns (uint256) {
        return FullMath.mulDiv(a, b, d);
    }

    function ourMulDiv(uint256 a, uint256 b, uint256 d) public pure returns (uint256) {
        return RationalToQ128x128.mulDivU(a, b, d);
    }

    // Signed wrapper using our implementation
    function ourMulDivSigned(int256 a, int256 b, int256 d) public pure returns (int256) {
        return RationalToQ128x128.mulDivS(a, b, d);
    }

    function _tryFull(uint256 a, uint256 b, uint256 d) internal returns (bool ok, uint256 res) {
        try this.fullMathMulDiv(a, b, d) returns (uint256 r) {
            return (true, r);
        } catch {
            return (false, 0);
        }
    }

    function _tryOur(uint256 a, uint256 b, uint256 d) internal returns (bool ok, uint256 res) {
        try this.ourMulDiv(a, b, d) returns (uint256 r) {
            return (true, r);
        } catch {
            return (false, 0);
        }
    }

    /// @notice Fuzz: compare success/failure and results across wide range of inputs
    function testFuzz_MulDiv_Agrees(uint256 a, uint256 b, uint256 d) public {
        vm.assume(d != 0); // both libs revert on div by zero; skip that here, tested separately below

        (bool okFull, uint256 rFull) = _tryFull(a, b, d);
        (bool okOur, uint256 rOur) = _tryOur(a, b, d);

        assertEq(okOur, okFull, "mulDiv success mismatch vs FullMath");
        if (okFull) {
            assertEq(rOur, rFull, "mulDiv result mismatch vs FullMath");
        }
    }

    /// @notice Specific: division by zero should revert in both implementations
    function test_MulDiv_DivByZeroReverts(uint256 a, uint256 b) public {
        // We can't easily match revert data, so simply assert both revert.
        // FullMath
        try this.fullMathMulDiv(a, b, 0) returns (uint256) {
            fail("FullMath.mulDiv did not revert on denominator == 0");
        } catch {}

        // Ours
        try this.ourMulDiv(a, b, 0) returns (uint256) {
            fail("RationalToQ128x128.mulDiv did not revert on denominator == 0");
        } catch {}
    }

    // -----------------------------
    // Signed mulDiv tests & edges
    // -----------------------------

    /// @notice Signed: division by zero should revert
    function test_SignedMulDiv_DivByZeroReverts(int256 a, int256 b) public {
        try this.ourMulDivSigned(a, b, 0) returns (int256) {
            fail("RationalToQ128x128.mulDiv (signed) did not revert on denominator == 0");
        } catch {}
    }

    /// @notice Signed: overflow due to magnitude exceeding int256.max (including sign-bit case)
    function test_SignedMulDiv_OverflowFromSignBit_MinIntTimes1Over1() public {
        // |INT256_MIN| = 2^255, so floor(2^255 * 1 / 1) = 2^255 which doesn't fit in int256
        try this.ourMulDivSigned(type(int256).min, int256(1), int256(1)) returns (int256) {
            fail("Expected signed mulDiv to revert due to signed overflow (2^255 magnitude)");
        } catch {}
    }

    /// @notice Signed: boundary values that should succeed
    function test_SignedMulDiv_BoundariesAndSigns() public {
        // Exact boundaries within range
        assertEq(this.ourMulDivSigned(type(int256).max, int256(1), int256(1)), type(int256).max);
        assertEq(this.ourMulDivSigned(-type(int256).max, int256(1), int256(1)), -type(int256).max);

        // Using INT256_MIN but scaling by 1/2 keeps magnitude within range
        int256 halfMinMag = -int256(uint256(1) << 254); // -2^254 fits in int256
        assertEq(this.ourMulDivSigned(type(int256).min, int256(1), int256(2)), halfMinMag);

        // Negative denominator flips the sign
        int256 halfMinMagPos = int256(uint256(1) << 254); // +2^254
        assertEq(this.ourMulDivSigned(type(int256).min, int256(1), int256(-2)), halfMinMagPos);
    }

    /// @notice Signed: truncation toward zero for negative results
    function test_SignedMulDiv_TruncatesTowardZero() public {
        // (-7 * 3) / 2 = -21/2 -> trunc toward zero = -10
        assertEq(this.ourMulDivSigned(-7, 3, 2), -10);

        // Mixed signs and non-exact division
        // (7 * -3) / 2 = -10 as well
        assertEq(this.ourMulDivSigned(7, -3, 2), -10);

        // (-7 * -3) / 2 = 21/2 -> 10
        assertEq(this.ourMulDivSigned(-7, -3, 2), 10);
    }

    /// @notice Deterministic cases around boundaries and simple arithmetic
    function test_KnownValues() public pure {
        // Simple exact divisions
        assertEq(RationalToQ128x128.mulDivU(6, 7, 3), FullMath.mulDiv(6, 7, 3)); // 42/3=14
        assertEq(RationalToQ128x128.mulDivU(1e18, 3e18, 1e18), FullMath.mulDiv(1e18, 3e18, 1e18)); // 3e18
        assertEq(RationalToQ128x128.mulDivU(0, type(uint256).max, 1234567), 0);

        // Powers of two denominators
        uint256 x = type(uint128).max;
        uint256 y = type(uint128).max;
        assertEq(RationalToQ128x128.mulDivU(x, y, 2), FullMath.mulDiv(x, y, 2));
        assertEq(RationalToQ128x128.mulDivU(x, y, 1 << 128), FullMath.mulDiv(x, y, 1 << 128));
    }

    /// @notice Cases that likely require the 512-bit path (prod1 != 0) when multiplying
    function test_UsesHighPrecisionPath() public {
        uint256 x = (uint256(1) << 200); // 2^200
        uint256 y = (uint256(1) << 80); // 2^80 -> product is 2^280
        uint256 d = (uint256(1) << 128); // 2^128

        (bool okFull, uint256 rFull) = _tryFull(x, y, d);
        (bool okOur, uint256 rOur) = _tryOur(x, y, d);
        assertEq(okOur, okFull, "success mismatch");
        if (okFull) {
            assertEq(rOur, rFull, "result mismatch");
        }
    }
}
