#ifndef NUMBER_THEORY_PRIME_H_
#define NUMBER_THEORY_PRIME_H_

#include <limits>
#include <type_traits>

// This file contains functions related to prime numbers.

namespace tql {
namespace number_theory {

// Tests whether |number| is prime or not.
//
// This overload is only available for numbers smaller than 2^16 since it
// performs poorly for large numbers.
template <typename T,
          std::enable_if_t<std::numeric_limits<T>::is_integer &&
                               std::numeric_limits<T>::digits <= 16,
                           bool> = true>
constexpr bool is_prime(T number) {
  // There are no prime numbers smaller than 2.
  if (number < 2)
    return false;
  for (int i = 2; i * i <= number; ++i) {
    if (number % i == 0)
      return false;
  }
  return true;
}

}  // namespace number_theory

using number_theory::is_prime;

}  // namespace tql

#endif  // NUMBER_THEORY_PRIME_H_
