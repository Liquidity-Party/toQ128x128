## RationalToQ128x128

**Specialized Solidity muldiv to create Q128.128 uints from a uint numerator and a uint denominator**

This implementation uses the same modular inverse technique pioneered by Uniswap for high-precision multiplication and division in Solidity. Rather than losing precision through separate operations, it computes the full 512-bit product of `numerator × 2^128`, then divides by the denominator using a modular inverse approach. The method factors out powers of two from the denominator, making it odd, then computes its modular inverse mod 2^256 using Newton-Raphson iteration. This allows the exact division result to be obtained through a single multiplication, avoiding overflow while maintaining full precision throughout the calculation.

The Q128.128 produced is a uint256 (unsigned)

## Test Cases

Install the Foundry Toolkit:
https://book.getfoundry.sh/

```shell
$ forge install
$ forge test
```
