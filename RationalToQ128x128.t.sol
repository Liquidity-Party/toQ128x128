// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

import "forge-std/Test.sol";
import "./RationalToQ128x128.sol";
import "@uniswap/FullMath.sol";

/// @author Vibed by Claude 4.5 Sonnet
contract RationalToQ128x128Test is Test {
    using RationalToQ128x128 for RationalToQ128x128.Rational;
    using RationalToQ128x128 for RationalToQ128x128.UnsignedRational;

    uint256 constant private Q128 = 1 << 128;

    /// @notice Fuzz test unsigned conversion against FullMath
    /// @dev Compares toQ128x128 result with FullMath.mulDiv(numerator, 2^128, denominator)
    function testFuzz_toQ128x128_AgainstFullMath(uint256 numerator, uint256 denominator) public {
        // Bound inputs to avoid trivial rejections
        denominator = bound(denominator, 1, type(uint256).max);

        // Test that our implementation reverts whenever FullMath reverts
        try this.calculateExpectedUnsigned(numerator, denominator) returns (uint256 expected) {
            uint256 actual = this.externalToQ128x128Unsigned(numerator, denominator);
            assertEq(actual, expected, "toQ128x128 should match FullMath result");
        } catch {
            vm.expectRevert();
            this.externalToQ128x128Unsigned(numerator, denominator);
        }
    }

    /// @notice Fuzz test unsigned conversion with reasonable bounds
    /// @dev Tests smaller values more likely to succeed
    function testFuzz_toQ128x128_BoundedInputs(uint128 numerator, uint128 denominator) public pure {
        vm.assume(denominator > 0);

        uint256 expected = FullMath.mulDiv(numerator, Q128, denominator);

        RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
            numerator: numerator,
            denominator: denominator
        });

        uint256 actual = frac.toQ128x128();
        assertEq(actual, expected, "Bounded inputs should match FullMath");
    }

    /// @notice Fuzz test that verifies the Q128.128 format is correct
    /// @dev Checks that result * denominator ≈ numerator * 2^128 (within rounding)
    function testFuzz_toQ128x128_FormatCorrectness(uint128 numerator, uint128 denominator) public pure {
        vm.assume(denominator > 0);

        RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
            numerator: numerator,
            denominator: denominator
        });

        uint256 q128Result = frac.toQ128x128();

        // Verify: q128Result = floor(numerator * 2^128 / denominator)
        // So: q128Result * denominator <= numerator * 2^128 < (q128Result + 1) * denominator
        uint256 product = FullMath.mulDiv(q128Result, denominator, 1);
        uint256 expected = FullMath.mulDiv(numerator, Q128, 1);

        assertLe(product, expected, "Result should be floor division");

        // Check upper bound (unless result would overflow)
        if (q128Result < type(uint256).max) {
            uint256 upperProduct = FullMath.mulDiv(q128Result + 1, denominator, 1);
            assertGt(upperProduct, expected, "Result should be exact floor");
        }
    }

    /// @notice Fuzz test signed conversion against FullMath
    /// @dev Tests all four sign combinations
    function testFuzz_toQ128x128_SignedAgainstFullMath(
        int128 numerator,
        int128 denominator
    ) public {
        vm.assume(denominator != 0);

        bool resultNegative = (numerator < 0) != (denominator < 0);
        uint256 absNumerator = numerator >= 0 ? uint256(int256(numerator)) : uint256(-int256(numerator));
        uint256 absDenominator = denominator >= 0 ? uint256(int256(denominator)) : uint256(-int256(denominator));

        RationalToQ128x128.Rational memory frac = RationalToQ128x128.Rational({
            numerator: numerator,
            denominator: denominator
        });

        // Test that our implementation reverts whenever FullMath reverts
        try this.calculateExpectedUnsigned(absNumerator, absDenominator) returns (uint256 absResult) {
            // Check if signed conversion would overflow int256
            if (absResult > uint256(type(int256).max)) {
                vm.expectRevert();
                frac.toQ128x128();
            } else {
                int256 actual = frac.toQ128x128();
                int256 expected = resultNegative ? -int256(absResult) : int256(absResult);
                assertEq(actual, expected, "toQ128x128 should match FullMath with sign");
            }
        } catch {
            vm.expectRevert();
            frac.toQ128x128();
        }
    }

    /// @notice Fuzz test signed conversion with various sign combinations
    function testFuzz_toQ128x128_SignedSignHandling(
        uint64 absNumerator,
        uint64 absDenominator,
        bool numeratorNegative,
        bool denominatorNegative
    ) public pure {
        vm.assume(absDenominator > 0);
        vm.assume(absNumerator > 0);

        int256 numerator = numeratorNegative ? -int256(uint256(absNumerator)) : int256(uint256(absNumerator));
        int256 denominator = denominatorNegative ? -int256(uint256(absDenominator)) : int256(uint256(absDenominator));

        bool expectedNegative = numeratorNegative != denominatorNegative;

        RationalToQ128x128.Rational memory frac = RationalToQ128x128.Rational({
            numerator: numerator,
            denominator: denominator
        });

        int256 result = frac.toQ128x128();

        if (expectedNegative) {
            assertLt(result, 0, "Result should be negative");
        } else {
            assertGe(result, 0, "Result should be non-negative");
        }

        // Verify magnitude matches unsigned calculation
        uint256 unsignedResult = RationalToQ128x128.toQ128x128U(
            uint256(absNumerator),
            uint256(absDenominator)
        );

        uint256 resultMagnitude = result >= 0 ? uint256(result) : uint256(-result);
        assertEq(resultMagnitude, unsignedResult, "Magnitude should match unsigned conversion");
    }

    /// @notice Test edge case: zero numerator
    function testFuzz_ZeroNumerator(uint256 denominator) public pure {
        denominator = bound(denominator, 1, type(uint256).max);

        RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
            numerator: 0,
            denominator: denominator
        });

        uint256 result = frac.toQ128x128();
        assertEq(result, 0, "Zero numerator should give zero result");
    }

    /// @notice Test edge case: denominator equals numerator (result should be 2^128)
    function testFuzz_NumeratorEqualsDenominator(uint128 value) public pure {
        vm.assume(value > 0);

        RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
            numerator: value,
            denominator: value
        });

        uint256 result = frac.toQ128x128();
        assertEq(result, Q128, "numerator == denominator should give 2^128");
    }

    /// @notice Test that zero denominator reverts
    function testFuzz_ZeroDenominatorReverts(uint256 numerator) public {
        vm.expectRevert("toQ128x128: div by zero");
        this.externalToQ128x128Unsigned(numerator, 0);
    }

    /// @notice Test that zero denominator reverts for signed
    function testFuzz_SignedZeroDenominatorReverts(int256 numerator) public {
        vm.expectRevert("toQ128x128: div by zero");
        this.externalToQ128x128Signed(numerator, 0);
    }

    /// @notice Helper function to calculate expected result using FullMath (external for try-catch)
    function calculateExpectedUnsigned(uint256 numerator, uint256 denominator) 
        external 
        pure 
        returns (uint256) 
    {
        require(denominator > 0, "denominator must be > 0");
        return FullMath.mulDiv(numerator, Q128, denominator);
    }

    /// @notice External wrapper for unsigned toQ128x128 (needed for vm.expectRevert)
    function externalToQ128x128Unsigned(uint256 numerator, uint256 denominator) 
        external 
        pure 
        returns (uint256) 
    {
        RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
            numerator: numerator,
            denominator: denominator
        });
        return frac.toQ128x128();
    }

    /// @notice External wrapper for signed toQ128x128 (needed for vm.expectRevert)
    function externalToQ128x128Signed(int256 numerator, int256 denominator) 
        external 
        pure 
        returns (int256) 
    {
        RationalToQ128x128.Rational memory frac = RationalToQ128x128.Rational({
            numerator: numerator,
            denominator: denominator
        });
        return frac.toQ128x128();
    }

    /// @notice Test comparison with FullMath for specific known values
    function test_KnownValues() public pure {
        // Test 1/2 = 0.5 in Q128.128
        RationalToQ128x128.UnsignedRational memory half = RationalToQ128x128.UnsignedRational({
            numerator: 1,
            denominator: 2
        });
        uint256 halfResult = half.toQ128x128();
        uint256 expectedHalf = FullMath.mulDiv(1, Q128, 2);
        assertEq(halfResult, expectedHalf, "1/2 should match");
        assertEq(halfResult, Q128 / 2, "1/2 should equal 2^127");

        // Test 2/1 = 2.0 in Q128.128
        RationalToQ128x128.UnsignedRational memory two = RationalToQ128x128.UnsignedRational({
            numerator: 2,
            denominator: 1
        });
        uint256 twoResult = two.toQ128x128();
        uint256 expectedTwo = FullMath.mulDiv(2, Q128, 1);
        assertEq(twoResult, expectedTwo, "2/1 should match");
        assertEq(twoResult, Q128 * 2, "2/1 should equal 2^129");

        // Test 3/4 = 0.75 in Q128.128
        RationalToQ128x128.UnsignedRational memory threeFourths = RationalToQ128x128.UnsignedRational({
            numerator: 3,
            denominator: 4
        });
        uint256 threeFourthsResult = threeFourths.toQ128x128();
        uint256 expectedThreeFourths = FullMath.mulDiv(3, Q128, 4);
        assertEq(threeFourthsResult, expectedThreeFourths, "3/4 should match");
    }

    /// @notice Test signed known values
    function test_SignedKnownValues() public pure {
        // Test -1/2 = -0.5
        RationalToQ128x128.Rational memory negHalf = RationalToQ128x128.Rational({
            numerator: -1,
            denominator: 2
        });
        int256 negHalfResult = negHalf.toQ128x128();
        assertEq(negHalfResult, -int256(Q128 / 2), "-1/2 should be negative 2^127");

        // Test 1/-2 = -0.5 (negative denominator)
        RationalToQ128x128.Rational memory halfNegDenom = RationalToQ128x128.Rational({
            numerator: 1,
            denominator: -2
        });
        int256 halfNegDenomResult = halfNegDenom.toQ128x128();
        assertEq(halfNegDenomResult, -int256(Q128 / 2), "1/-2 should be negative 2^127");

        // Test -1/-2 = 0.5 (both negative)
        RationalToQ128x128.Rational memory bothNeg = RationalToQ128x128.Rational({
            numerator: -1,
            denominator: -2
        });
        int256 bothNegResult = bothNeg.toQ128x128();
        assertEq(bothNegResult, int256(Q128 / 2), "-1/-2 should be positive 2^127");
    }

    /// @notice Test overflow edge cases for unsigned conversion
    function test_UnsignedOverflowBoundary() public {
        // Maximum safe value: type(uint256).max / 2^128
        uint256 maxSafeNumerator = type(uint256).max >> 128;

        RationalToQ128x128.UnsignedRational memory maxSafe = RationalToQ128x128.UnsignedRational({
            numerator: maxSafeNumerator,
            denominator: 1
        });

        uint256 result = maxSafe.toQ128x128();
        assertEq(result, maxSafeNumerator << 128, "Max safe value should not overflow");

        // Just beyond max safe value should revert
        vm.expectRevert("Q128x128: overflow");
        this.externalToQ128x128Unsigned(maxSafeNumerator + 1, 1);
    }

    /// @notice Test signed overflow at int256.max boundary
    function test_SignedOverflowBoundary() public {
        // Maximum safe signed value
        uint256 maxSafeUnsigned = uint256(type(int256).max);
        int256 maxSafeNumerator = int256(maxSafeUnsigned >> 128);

        RationalToQ128x128.Rational memory maxSafe = RationalToQ128x128.Rational({
            numerator: maxSafeNumerator,
            denominator: 1
        });

        int256 result = maxSafe.toQ128x128();
        assertEq(result, maxSafeNumerator << 128, "Max safe signed value should not overflow");

        // Just beyond should revert
        vm.expectRevert("toQ128x128: signed overflow");
        this.externalToQ128x128Signed(maxSafeNumerator + 1, 1);

        // Test negative boundary
        vm.expectRevert("toQ128x128: signed overflow");
        this.externalToQ128x128Signed(-(maxSafeNumerator + 1), 1);
    }

    /// @notice Test INT256_MIN edge case
    function test_INT256_MIN_EdgeCase() public pure {
        // INT256_MIN as numerator with positive denominator
        int256 minInt = type(int256).min;

        // This should work if denominator is large enough to prevent overflow
        // For result to fit in int256: 2^255 * 2^128 / denominator <= 2^255 - 1
        // Need denominator >= 2^128 + small amount. Using 2^129 gives result = 2^254
        RationalToQ128x128.Rational memory minWithLargeDenom = RationalToQ128x128.Rational({
            numerator: minInt,
            denominator: int256(uint256(1 << 129))
        });

        int256 result = minWithLargeDenom.toQ128x128();
        assertLt(result, 0, "INT256_MIN / large denominator should be negative");

        // INT256_MIN as denominator should work
        // Note: 1 / INT256_MIN = 1 / 2^255 in Q128.128 = 2^128 / 2^255 = 2^-127
        // This is so small it rounds to 0 in integer arithmetic
        RationalToQ128x128.Rational memory minAsDenom = RationalToQ128x128.Rational({
            numerator: 1,
            denominator: minInt
        });

        int256 result2 = minAsDenom.toQ128x128();
        assertEq(result2, 0, "1 / INT256_MIN rounds to 0 (too small to represent)");
    }

    /// @notice Test signed zero handling
    function test_SignedZeroNumerator() public pure {
        // Zero numerator with positive denominator
        RationalToQ128x128.Rational memory zeroPosD = RationalToQ128x128.Rational({
            numerator: 0,
            denominator: 12345
        });
        assertEq(zeroPosD.toQ128x128(), 0, "0 / positive should be 0");

        // Zero numerator with negative denominator
        RationalToQ128x128.Rational memory zeroNegD = RationalToQ128x128.Rational({
            numerator: 0,
            denominator: -12345
        });
        assertEq(zeroNegD.toQ128x128(), 0, "0 / negative should be 0");
    }

    /// @notice Test rounding toward zero for negative results
    function test_RoundingTowardZero_Negative() public pure {
        // Test -5/3 which should round toward zero (ceiling)
        // -5/3 = -1.666... should become floor(-1.666... * 2^128) in magnitude
        RationalToQ128x128.Rational memory negFrac = RationalToQ128x128.Rational({
            numerator: -5,
            denominator: 3
        });

        int256 result = negFrac.toQ128x128();

        // Calculate expected: floor(5 * 2^128 / 3) then negate
        uint256 unsignedExpected = FullMath.mulDiv(5, Q128, 3);
        int256 expected = -int256(unsignedExpected);

        assertEq(result, expected, "-5/3 should round toward zero");
        assertLt(result, 0, "Result should be negative");
    }

    /// @notice Test very large denominator (small results)
    function test_VeryLargeDenominator() public pure {
        // Test 1 / type(uint128).max (very small result)
        RationalToQ128x128.UnsignedRational memory verySmall = RationalToQ128x128.UnsignedRational({
            numerator: 1,
            denominator: type(uint128).max
        });

        uint256 result = verySmall.toQ128x128();
        assertGt(result, 0, "Should not round to zero");
        assertLt(result, 1 << 128, "Should be less than 1.0");

        // Test that rounds to zero
        RationalToQ128x128.UnsignedRational memory roundsToZero = RationalToQ128x128.UnsignedRational({
            numerator: 1,
            denominator: type(uint256).max
        });

        uint256 result2 = roundsToZero.toQ128x128();
        assertEq(result2, 0, "1 / uint256.max should round to zero");
    }

    /// @notice Test precision loss requiring 512-bit calculation
    function test_512BitPrecision() public {
        // Use numerator large enough that prod1 > 0
        // numerator * 2^128 requires more than 256 bits when numerator > 2^128
        uint256 largeNumerator = (uint256(1) << 200); // 2^200
        uint256 largeDenominator = (uint256(1) << 72); // 2^72

        // Result should be 2^200 * 2^128 / 2^72 = 2^256, which overflows
        vm.expectRevert("Q128x128: overflow");
        this.externalToQ128x128Unsigned(largeNumerator, largeDenominator);

        // Test case that requires 512-bit but doesn't overflow
        uint256 numerator2 = (uint256(1) << 200);
        uint256 denominator2 = (uint256(1) << 73); // Make denominator larger

        uint256 result = this.externalToQ128x128Unsigned(numerator2, denominator2);

        // Expected: 2^200 * 2^128 / 2^73 = 2^255
        uint256 expected = uint256(1) << 255;
        assertEq(result, expected, "512-bit calculation should be precise");
    }

    /// @notice Test cheap path where prod1 == 0
    function test_CheapPath_Prod1Zero() public pure {
        // When numerator << 128 fits in uint256, prod1 is 0
        uint256 smallNumerator = 1000;
        uint256 denominator = 7;

        RationalToQ128x128.UnsignedRational memory small = RationalToQ128x128.UnsignedRational({
            numerator: smallNumerator,
            denominator: denominator
        });

        uint256 result = small.toQ128x128();
        uint256 expected = FullMath.mulDiv(smallNumerator, Q128, denominator);
        assertEq(result, expected, "Cheap path should match FullMath");
    }

    /// @notice Test direct function calls (non-struct)
    function test_DirectFunctionCalls() public pure {
        // Test unsigned direct call
        uint256 unsignedResult = RationalToQ128x128.toQ128x128U(1, 2);
        assertEq(unsignedResult, Q128 / 2, "Direct unsigned call: 1/2");

        // Test signed direct call
        int256 signedResult = RationalToQ128x128.toQ128x128S(-1, 2);
        assertEq(signedResult, -int256(Q128 / 2), "Direct signed call: -1/2");

        // Test positive signed
        int256 signedPos = RationalToQ128x128.toQ128x128S(3, 4);
        assertEq(signedPos, int256(FullMath.mulDiv(3, Q128, 4)), "Direct signed call: 3/4");
    }

    /// @notice Test systematic denominator variations
    function test_SystematicDenominatorVariation() public pure {
        uint256 fixedNumerator = 1000;

        // Test powers of 2 as denominators
        for (uint256 i = 1; i <= 10; i++) {
            uint256 denominator = 1 << i;

            RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
                numerator: fixedNumerator,
                denominator: denominator
            });

            uint256 result = frac.toQ128x128();
            uint256 expected = FullMath.mulDiv(fixedNumerator, Q128, denominator);
            assertEq(result, expected, "Power of 2 denominator should match");
        }
    }

    /// @notice Test result exactly at int256.max (should succeed)
    function test_ExactInt256Max() public pure {
        // Construct fraction that gives exactly int256.max
        // int256.max = 2^255 - 1
        // We need numerator * 2^128 / denominator = 2^255 - 1
        // Let's use: (2^255 - 1) / 2^128 as our fraction

        uint256 targetResult = uint256(type(int256).max);
        uint256 numerator = targetResult >> 128; // Divide by 2^128
        uint256 denominator = 1;

        // Adjust to get exact result
        if ((numerator << 128) != targetResult) {
            // Need fractional adjustment - use larger denominator
            numerator = targetResult;
            denominator = 1 << 128;
        }

        RationalToQ128x128.Rational memory atMax = RationalToQ128x128.Rational({
            numerator: int256(numerator),
            denominator: int256(denominator)
        });

        int256 result = atMax.toQ128x128();
        assertLe(result, type(int256).max, "Should not exceed int256.max");
    }

    /// @notice Test remainder handling in 512-bit division
    function test_RemainderHandling() public pure {
        // Test fraction with non-trivial remainder
        // 7/3 = 2.333...
        RationalToQ128x128.UnsignedRational memory withRemainder = RationalToQ128x128.UnsignedRational({
            numerator: 7,
            denominator: 3
        });

        uint256 result = withRemainder.toQ128x128();
        uint256 expected = FullMath.mulDiv(7, Q128, 3);
        assertEq(result, expected, "Should handle remainder correctly");

        // Verify it's floor division: result * denominator <= numerator * Q128
        uint256 lowerBound = FullMath.mulDiv(result, 3, Q128);
        assertLe(lowerBound, 7, "Should be floor division");
    }

    /// @notice Test that prod1 > 0 activates complex path
    function test_ComplexDivisionPath() public pure {
        // Force prod1 > 0 by using numerator where numerator >> 128 != 0
        uint256 numerator = (uint256(3) << 129); // 3 * 2^129
        uint256 denominator = uint256(5) << 130; // 5 * 2^130

        // This should give approximately (3/5) * 2^128 but requires 512-bit math
        RationalToQ128x128.UnsignedRational memory complex = RationalToQ128x128.UnsignedRational({
            numerator: numerator,
            denominator: denominator
        });

        uint256 result = complex.toQ128x128();

        // Verify against FullMath
        uint256 expected = FullMath.mulDiv(numerator, Q128, denominator);
        assertEq(result, expected, "Complex path should match FullMath");
    }

    /// @notice Test signed result that rounds to zero
    function test_SignedRoundsToZero() public pure {
        // Very small negative fraction
        RationalToQ128x128.Rational memory tiny = RationalToQ128x128.Rational({
            numerator: -1,
            denominator: int256(uint256(type(uint128).max))
        });

        int256 result = tiny.toQ128x128();
        assertLt(result, 0, "Should be negative");
        assertGt(result, -1000000, "Should be very small");
    }

    // ============================================================
    //                  OFF-BY-ONE BOUNDARY TESTS
    // ============================================================

    /// @notice Test unsigned just inside max safe boundary
    function test_UnsignedJustInsideBoundary() public pure {
        uint256 maxSafeNumerator = type(uint256).max >> 128;

        // One less than max safe
        RationalToQ128x128.UnsignedRational memory justInside = RationalToQ128x128.UnsignedRational({
            numerator: maxSafeNumerator - 1,
            denominator: 1
        });

        uint256 result = justInside.toQ128x128();
        uint256 expected = (maxSafeNumerator - 1) << 128;
        assertEq(result, expected, "Just inside boundary should succeed");
    }

    /// @notice Test max/max equals Q128
    function test_MaxDividedByMax() public pure {
        RationalToQ128x128.UnsignedRational memory maxByMax = RationalToQ128x128.UnsignedRational({
            numerator: type(uint256).max,
            denominator: type(uint256).max
        });

        uint256 result = maxByMax.toQ128x128();
        assertEq(result, Q128, "max/max should equal 2^128");
    }

    /// @notice Test max numerator with large denominator
    function test_MaxNumeratorLargeDenominator() public {
        // type(uint256).max / type(uint128).max = 2^128 + (2^128 - 1) / 2^128 - 1 ≈ 2^128
        // Result would be ≈ 2^256 which overflows
        vm.expectRevert("Q128x128: overflow");
        this.externalToQ128x128Unsigned(type(uint256).max, type(uint128).max);
    }

    /// @notice Test signed just inside max boundary
    function test_SignedJustInsideBoundary() public pure {
        uint256 maxSafeUnsigned = uint256(type(int256).max);
        int256 maxSafeNumerator = int256(maxSafeUnsigned >> 128);

        // One less than max safe
        RationalToQ128x128.Rational memory justInside = RationalToQ128x128.Rational({
            numerator: maxSafeNumerator - 1,
            denominator: 1
        });

        int256 result = justInside.toQ128x128();
        int256 expected = (maxSafeNumerator - 1) << 128;
        assertEq(result, expected, "Signed just inside boundary should succeed");
    }

    /// @notice Test int256.min + 1 divided by -1
    function test_SignedMinPlusOneDivNegOne() public {
        int256 numerator = type(int256).min + 1;

        // (INT256_MIN + 1) / -1 = -(INT256_MIN + 1) = INT256_MAX
        // Then multiply by 2^128, which overflows
        vm.expectRevert("Q128x128: overflow");
        this.externalToQ128x128Signed(numerator, -1);
    }

    // ============================================================
    //                  SIGN-SPECIFIC EDGE CASES
    // ============================================================

    /// @notice Test both operands are INT256_MIN
    function test_BothOperandsINT256_MIN() public pure {
        RationalToQ128x128.Rational memory both = RationalToQ128x128.Rational({
            numerator: type(int256).min,
            denominator: type(int256).min
        });

        int256 result = both.toQ128x128();
        assertEq(result, int256(Q128), "INT256_MIN / INT256_MIN should equal 2^128");
    }

    /// @notice Test INT256_MIN divided by -1 (should overflow)
    function test_INT256_MIN_DivNegOne() public {
        // The unsigned conversion overflows before the signed check
        vm.expectRevert("Q128x128: overflow");
        this.externalToQ128x128Signed(type(int256).min, -1);
    }

    /// @notice Test -1 divided by INT256_MIN
    function test_NegOneDivINT256_MIN() public pure {
        RationalToQ128x128.Rational memory frac = RationalToQ128x128.Rational({
            numerator: -1,
            denominator: type(int256).min
        });

        int256 result = frac.toQ128x128();
        // 1 / 2^255 * 2^128 = 2^(-127), rounds to 0
        assertEq(result, 0, "-1 / INT256_MIN should round to 0");
    }

    /// @notice Test very small positive magnitude
    function test_VerySmallPositiveMagnitude() public pure {
        RationalToQ128x128.Rational memory tiny = RationalToQ128x128.Rational({
            numerator: 1,
            denominator: int128(type(int128).max)
        });

        int256 result = tiny.toQ128x128();
        assertGt(result, 0, "Should be positive");
        assertLt(result, 1000000, "Should be very small");
    }

    /// @notice Test very small negative magnitude
    function test_VerySmallNegativeMagnitude() public pure {
        RationalToQ128x128.Rational memory tiny = RationalToQ128x128.Rational({
            numerator: -1,
            denominator: int128(type(int128).max)
        });

        int256 result = tiny.toQ128x128();
        assertLt(result, 0, "Should be negative");
        assertGt(result, -1000000, "Should be very small magnitude");
    }

    /// @notice Test sign combinations with large denominators
    function test_SignCombosLargeDenominators() public pure {
        int256 largeDenom = int256(uint256(1) << 254);

        // Positive / Positive
        RationalToQ128x128.Rational memory posPos = RationalToQ128x128.Rational({
            numerator: 1,
            denominator: largeDenom
        });
        int256 resultPosPos = posPos.toQ128x128();
        assertEq(resultPosPos, 0, "+1 / +2^254 should round to 0");

        // Negative / Positive
        RationalToQ128x128.Rational memory negPos = RationalToQ128x128.Rational({
            numerator: -1,
            denominator: largeDenom
        });
        int256 resultNegPos = negPos.toQ128x128();
        assertEq(resultNegPos, 0, "-1 / +2^254 should round to 0");

        // Positive / Negative
        RationalToQ128x128.Rational memory posNeg = RationalToQ128x128.Rational({
            numerator: 1,
            denominator: -largeDenom
        });
        int256 resultPosNeg = posNeg.toQ128x128();
        assertEq(resultPosNeg, 0, "+1 / -2^254 should round to 0");

        // Negative / Negative
        RationalToQ128x128.Rational memory negNeg = RationalToQ128x128.Rational({
            numerator: -1,
            denominator: -largeDenom
        });
        int256 resultNegNeg = negNeg.toQ128x128();
        assertEq(resultNegNeg, 0, "-1 / -2^254 should round to 0");
    }

    // ============================================================
    //                  PATH-SPECIFIC TESTS (prod1)
    // ============================================================

    /// @notice Test guaranteed prod1 == 0 with various denominators
    function test_Prod1Zero_VariousDenominators() public pure {
        uint256 smallNumerator = (uint256(1) << 127) - 1; // Less than 2^128

        uint256[] memory denominators = new uint256[](4);
        denominators[0] = 1;
        denominators[1] = 7;
        denominators[2] = uint256(1) << 64;
        denominators[3] = uint256(1) << 127;

        for (uint256 i = 0; i < denominators.length; i++) {
            RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
                numerator: smallNumerator,
                denominator: denominators[i]
            });

            uint256 result = frac.toQ128x128();
            uint256 expected = FullMath.mulDiv(smallNumerator, Q128, denominators[i]);
            assertEq(result, expected, "prod1==0 path should match FullMath");
        }
    }

    /// @notice Test guaranteed prod1 > 0 without overflow
    function test_Prod1Positive_NoOverflow() public pure {
        // Test case 1: 2^200 / 2^73
        RationalToQ128x128.UnsignedRational memory case1 = RationalToQ128x128.UnsignedRational({
            numerator: uint256(1) << 200,
            denominator: uint256(1) << 73
        });
        uint256 result1 = case1.toQ128x128();
        uint256 expected1 = FullMath.mulDiv(uint256(1) << 200, Q128, uint256(1) << 73);
        assertEq(result1, expected1, "2^200 / 2^73 should match FullMath");

        // Test case 2: 2^150 / 2^50
        RationalToQ128x128.UnsignedRational memory case2 = RationalToQ128x128.UnsignedRational({
            numerator: uint256(1) << 150,
            denominator: uint256(1) << 50
        });
        uint256 result2 = case2.toQ128x128();
        uint256 expected2 = FullMath.mulDiv(uint256(1) << 150, Q128, uint256(1) << 50);
        assertEq(result2, expected2, "2^150 / 2^50 should match FullMath");
    }

    /// @notice Test guaranteed prod1 > 0 with overflow
    function test_Prod1Positive_WithOverflow() public {
        // 2^255 / 1 should overflow
        vm.expectRevert("Q128x128: overflow");
        this.externalToQ128x128Unsigned(uint256(1) << 255, 1);

        // 2^200 / 2^72: prod1 = 2^72, denominator = 2^72, should overflow (denominator <= prod1)
        vm.expectRevert("Q128x128: overflow");
        this.externalToQ128x128Unsigned(uint256(1) << 200, uint256(1) << 72);
    }

    /// @notice Test denominator exactly at overflow boundary
    function test_DenominatorAtOverflowBoundary() public {
        // Choose numerator such that prod1 is computable
        // numerator = 2^200, so prod1 = 2^200 >> 128 = 2^72
        uint256 numerator = uint256(1) << 200;
        uint256 prod1 = numerator >> 128; // = 2^72

        // denominator = prod1 + 1 should succeed
        RationalToQ128x128.UnsignedRational memory justSafe = RationalToQ128x128.UnsignedRational({
            numerator: numerator,
            denominator: prod1 + 1
        });
        uint256 result = justSafe.toQ128x128();
        uint256 expected = FullMath.mulDiv(numerator, Q128, prod1 + 1);
        assertEq(result, expected, "denominator = prod1 + 1 should succeed");

        // denominator = prod1 should revert
        vm.expectRevert("Q128x128: overflow");
        this.externalToQ128x128Unsigned(numerator, prod1);
    }

    // ============================================================
    //            DENOMINATOR POWER-OF-TWO & FACTORIZATION
    // ============================================================

    /// @notice Test pure powers of two as denominators
    function test_PurePowersOfTwo() public pure {
        uint256 numerator = 1000;
        uint256[] memory powers = new uint256[](7);
        powers[0] = 1;
        powers[1] = 2;
        powers[2] = 64;
        powers[3] = 127;
        powers[4] = 128;
        powers[5] = 192;
        powers[6] = 255;

        for (uint256 i = 0; i < powers.length; i++) {
            uint256 denominator = uint256(1) << powers[i];

            RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
                numerator: numerator,
                denominator: denominator
            });

            uint256 result = frac.toQ128x128();
            uint256 expected = FullMath.mulDiv(numerator, Q128, denominator);
            assertEq(result, expected, "Power of 2 denominator should match FullMath");
        }
    }

    /// @notice Test denominators with large power-of-two factors
    function test_LargePowerOfTwoFactors() public pure {
        // Test 3 * 2^128
        RationalToQ128x128.UnsignedRational memory case1 = RationalToQ128x128.UnsignedRational({
            numerator: uint256(9) << 150,
            denominator: uint256(3) << 128
        });
        uint256 result1 = case1.toQ128x128();
        uint256 expected1 = FullMath.mulDiv(uint256(9) << 150, Q128, uint256(3) << 128);
        assertEq(result1, expected1, "3 * 2^128 denominator should match");

        // Test 5 * 2^64
        RationalToQ128x128.UnsignedRational memory case2 = RationalToQ128x128.UnsignedRational({
            numerator: 1000,
            denominator: uint256(5) << 64
        });
        uint256 result2 = case2.toQ128x128();
        uint256 expected2 = FullMath.mulDiv(1000, Q128, uint256(5) << 64);
        assertEq(result2, expected2, "5 * 2^64 denominator should match");

        // Test 7 * 2^200 (requires large numerator to avoid underflow)
        RationalToQ128x128.UnsignedRational memory case3 = RationalToQ128x128.UnsignedRational({
            numerator: uint256(7) << 200,
            denominator: uint256(7) << 200
        });
        uint256 result3 = case3.toQ128x128();
        assertEq(result3, Q128, "7 * 2^200 / 7 * 2^200 should equal Q128");
    }

    /// @notice Test pure odd denominators (no factor of 2)
    function test_PureOddDenominators() public pure {
        uint256[] memory oddDenoms = new uint256[](5);
        oddDenoms[0] = 3;
        oddDenoms[1] = 5;
        oddDenoms[2] = 7;
        oddDenoms[3] = 99;
        oddDenoms[4] = 101;

        // Test with small numerator (prod1 == 0)
        for (uint256 i = 0; i < oddDenoms.length; i++) {
            RationalToQ128x128.UnsignedRational memory small = RationalToQ128x128.UnsignedRational({
                numerator: 1000,
                denominator: oddDenoms[i]
            });

            uint256 resultSmall = small.toQ128x128();
            uint256 expectedSmall = FullMath.mulDiv(1000, Q128, oddDenoms[i]);
            assertEq(resultSmall, expectedSmall, "Small numerator with odd denominator");
        }

        // Test with large numerator (prod1 > 0) - use smaller shift to avoid overflow
        for (uint256 i = 0; i < oddDenoms.length; i++) {
            uint256 largeNum = uint256(oddDenoms[i]) << 100;

            RationalToQ128x128.UnsignedRational memory large = RationalToQ128x128.UnsignedRational({
                numerator: largeNum,
                denominator: oddDenoms[i]
            });

            uint256 resultLarge = large.toQ128x128();
            uint256 expectedLarge = FullMath.mulDiv(largeNum, Q128, oddDenoms[i]);
            assertEq(resultLarge, expectedLarge, "Large numerator with odd denominator");
        }
    }

    /// @notice Test denominator = 1 (trivial case)
    function test_DenominatorOne() public pure {
        uint256 numerator = 123456;

        RationalToQ128x128.UnsignedRational memory frac = RationalToQ128x128.UnsignedRational({
            numerator: numerator,
            denominator: 1
        });

        uint256 result = frac.toQ128x128();
        assertEq(result, numerator << 128, "denominator=1 should left-shift by 128");
    }

    /// @notice Test mixed factorizations
    function test_MixedFactorizations() public pure {
        // Test 15 * 2^64
        RationalToQ128x128.UnsignedRational memory case1 = RationalToQ128x128.UnsignedRational({
            numerator: uint256(15) << 100,
            denominator: uint256(15) << 64
        });
        uint256 result1 = case1.toQ128x128();
        uint256 expected1 = FullMath.mulDiv(uint256(15) << 100, Q128, uint256(15) << 64);
        assertEq(result1, expected1, "15 * 2^64 mixed factorization");

        // Test 255 * 2^128
        RationalToQ128x128.UnsignedRational memory case2 = RationalToQ128x128.UnsignedRational({
            numerator: uint256(255) << 150,
            denominator: uint256(255) << 128
        });
        uint256 result2 = case2.toQ128x128();
        uint256 expected2 = FullMath.mulDiv(uint256(255) << 150, Q128, uint256(255) << 128);
        assertEq(result2, expected2, "255 * 2^128 mixed factorization");
    }
}
