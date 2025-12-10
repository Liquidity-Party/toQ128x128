## RationalToQ128x128

**Specialized Solidity muldiv to create Q128.128 numbers from a 256-bit numerator and denominator**

This implementation uses the same modular inverse technique pioneered by Uniswap for high-precision multiplication and division in Solidity. Rather than losing precision through separate operations, it computes the full 512-bit product of `numerator × 2^128`, then divides by the denominator using a modular inverse approach. The method factors out powers of two from the denominator, making it odd, then computes its modular inverse mod 2^256 using Newton-Raphson iteration. This allows the exact division result to be obtained through a single multiplication, avoiding overflow while maintaining full precision throughout the calculation.

**General muldiv**

There is also a general 512-bit muldiv included, so the community has an MIT-licensed alternative to Uniswap's FullMath. For creating Q128.128's, the specialized muldiv uses less gas and should be preferred.

**Usage**

`RationalToQ128x128.sol` is a standalone file with zero dependencies.

For testing, we compare the behavior of the new code with the established known-good Uniswap `FullMath` implementation, as revised for Solidity 0.8. This 0.8 version of `FullMath` relies on a fixed-point library from `solady`, so we have copied these test dependencies into `uni-v3-lib/` and `solady/`, which are licensed separately.

## Test Cases

Install the Foundry Toolkit:
https://book.getfoundry.sh/

```shell
$ forge install
$ forge test
```
