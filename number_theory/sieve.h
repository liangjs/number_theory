#ifndef NUMBER_THEORY_SIEVE_H_
#define NUMBER_THEORY_SIEVE_H_

#include <stddef.h>
#include <stdint.h>

#include <bit>
#include <limits>
#include <stdexcept>
#include <vector>

#include "number_theory/utility.h"

namespace tql {
namespace number_theory {

// Sieve of Euler.
// It finds all prime numbers under a certain limit.
// It also provides the factorizations of all numbers under the limit.
//
// The numbers are of type |T|, but they are converted to type |U| during
// multiplication to avoid overflow.
template <typename T, typename U = uint64_t>
class Sieve {
  static_assert(std::numeric_limits<T>::is_integer &&
                    std::numeric_limits<U>::is_integer,
                "Sieve must use integer types.");

 public:
  // The type of numbers used by the Sieve.
  using type = T;

  // Constructs the Sieve in linear time.
  explicit Sieve(T num_limit)
      : num_limit_(std::move(num_limit)), min_prime_factor_(num_limit_, T(0)) {
    // Detect overflow.
    size_t num_limit_width =
        std::bit_width(static_cast<std::make_unsigned_t<T>>(num_limit_));
    if (num_limit_width * 2 > std::numeric_limits<U>::digits)
      throw std::overflow_error(
          "Multiplication will overflow when sieving. "
          "Please use larger integer types.");

    // Euler's Sieve algorithm
    for (T num = 2; num < num_limit_; ++num) {
      if (min_prime_factor_[num] == 0) {
        primes_.push_back(num);
        min_prime_factor_[num] = num;
      }
      for (const T &prime : primes_) {
        if (prime > min_prime_factor_[num])
          break;
        U x = static_cast<U>(prime) * static_cast<U>(num);
        if (x >= static_cast<U>(num_limit_))
          break;
        min_prime_factor_[x] = prime;
      }
    }
  }

  Sieve(const Sieve &other) = default;
  Sieve(Sieve &&other) = default;
  Sieve &operator=(const Sieve &other) = default;
  Sieve &operator=(Sieve &&other) = default;

  // Returns the list of primes.
  const std::vector<T> &primes() const { return primes_; }

  // Returns the minimum prime factor of the |number|.
  T min_prime_factor(const T &number) const {
    auto abs_num = unsigned_abs(number);
    if (abs_num <= 1)
      throw std::domain_error("Minimum prime factor does not exist.");
    if (abs_num >= num_limit_)
      throw std::out_of_range("The number exceeds the limit of Sieve.");
    return min_prime_factor_[abs_num];
  }

 private:
  // The maximum number (exclusive) we can hold.
  T num_limit_;
  // Minimum prime factors.
  std::vector<T> min_prime_factor_;
  // Prime numbers.
  std::vector<T> primes_;
};

}  // namespace number_theory

using number_theory::Sieve;

}  // namespace tql

#endif  // NUMBER_THEORY_SIEVE_H_
