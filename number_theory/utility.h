#ifndef NUMBER_THEORY_UTILITY_H_
#define NUMBER_THEORY_UTILITY_H_

#include <functional>
#include <limits>
#include <type_traits>

// Public utility functions.

namespace tql {
namespace number_theory {

// Accumulates values according to the binary representation of |binary|.
// If |binary| is negative, the two's complement representation will be used.
// For each bit of |binary|, from lower to higher, it applies |operation| to
// accumulate the values. The initial value is |initial_value|.
// As an example, when |operation| is addition, it becomes a popcount.
template <typename T, typename U>
U binary_accumulate(T binary,
                    const U &initial_value,
                    std::function<void(bool, U &)> operation) {
  static_assert(std::numeric_limits<T>::is_integer,
                "binary_accumulate argument |binary| must be an integer");

  T current = binary;
  U result = initial_value;
  while (current != 0) {
    bool bit = (current % 2) != 0;
    current /= 2;
    operation(bit, result);
  }
  return result;
}

// Returns the sign of the number |x|.
// If x = 0, sign(x) = 0.
// If x > 0, sign(x) = 1.
// If x < 0, sign(x) = -1.
template <typename T>
constexpr int sign(T x) {
  if (x == 0)
    return 0;
  return x > 0 ? 1 : -1;
}

// Returns the absolute value of the integer |x| in an unsigned type.
//
// This can be useful because std::abs doesn't support unsigned integers, and
// std::abs(std::numeric_limits<T>::min()) is undefined.
template <typename T>
constexpr std::make_unsigned_t<T> unsigned_abs(T x) {
  static_assert(std::numeric_limits<T>::is_integer,
                "unsigned_abs argument |x| must be an integer.");

  auto y = static_cast<std::make_unsigned_t<T>>(x);
  return x < 0 ? -y : y;
}

}  // namespace number_theory
}  // namespace tql

#endif  // NUMBER_THEORY_UTILITY_H_
