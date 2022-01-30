#ifndef NUMBER_THEORY_NUMERIC_H_
#define NUMBER_THEORY_NUMERIC_H_

#include <cmath>
#include <functional>
#include <limits>
#include <numeric>
#include <type_traits>
#include <utility>

#include "number_theory/utility.h"

// Common numeric functions.

namespace tql {
namespace number_theory {

using std::gcd;
using std::lcm;

// Extended Euclidean algorithm.
// Given two numbers |a| and |b|, returns a pair (x, y) satisfying
// x*a + y*b == gcd(a,b).
template <typename T>
std::pair<T, T> exgcd(T a, T b) {
  static_assert(
      std::numeric_limits<T>::is_integer && std::numeric_limits<T>::is_signed,
      "exgcd arguments must be signed integers");

  // xa * abs(a) + ya * abs(b) == ta
  // xb * abs(b) + yb * abs(b) == tb
  auto ta = unsigned_abs(a), tb = unsigned_abs(b);
  T xa = 1, ya = 0;
  T xb = 0, yb = 1;

  while (tb != 0) {
    T q = ta / tb;

    // xc * abs(a) + yc * abs(b) == tc
    T tc = ta - q * tb;
    T xc = xa - q * xb;
    T yc = ya - q * yb;

    xa = xb, xb = xc;
    ya = yb, yb = yc;
    ta = tb, tb = tc;
  }

  return std::make_pair(sign(a) * xa, sign(b) * ya);
}

// Computes the value of |base| raised to an integer power |exponent|.
// This version of overload uses binary exponentiation technique to compute in
// O(log |exponent|) time.
template <typename T,
          typename U,
          std::enable_if_t<std::numeric_limits<U>::is_integer, bool> = true>
T pow(const T &base, U exponent) {
  using Pair_TT = std::pair<T, T>;
  using Unsigned_U = std::make_unsigned_t<U>;

  auto update = [](bool bit, Pair_TT &state) {
    // At the n-th bit of |exponent|, |power| is |base|^(2^n), and
    // |result| is the answer for the first n bits of |exponent|.
    auto &[result, power] = state;
    if (bit)
      result *= power;
    power *= power;
  };

  Unsigned_U abs_exp = unsigned_abs(exponent);
  Pair_TT state = binary_accumulate<Unsigned_U, Pair_TT>(
      abs_exp, std::make_pair(1, base), update);
  T result = std::move(state.first);

  // Return the inverse of the result if the exponent is negative.
  if (exponent < 0)
    return 1 / result;
  return result;
}

// Computes the value of |base| raised to a non-integer power |exponent|.
// This version of overload is the same as std::pow.
template <typename T,
          typename U,
          std::enable_if_t<!std::numeric_limits<U>::is_integer, bool> = true>
auto pow(T base, U exponent) -> decltype(std::pow(base, exponent)) {
  return std::pow(base, exponent);
}

}  // namespace number_theory

using number_theory::exgcd;
using number_theory::gcd;
using number_theory::lcm;
using number_theory::pow;

}  // namespace tql

#endif  // NUMBER_THEORY_NUMERIC_H_
