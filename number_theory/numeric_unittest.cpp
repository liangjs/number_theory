#include <stdint.h>

#include <cmath>
#include <limits>
#include <numeric>
#include <random>

#include <gtest/gtest.h>

#include "number_theory/numeric.h"

namespace tql {
namespace number_theory {

template <typename T>
void test_exgcd(T a, T b) {
  auto [x, y] = exgcd(a, b);
  if (a != 0 && b != 0) {
    // Bézout's identity
    ASSERT_LE(unsigned_abs(x), unsigned_abs(b));
    ASSERT_LE(unsigned_abs(y), unsigned_abs(a));
  }
  ASSERT_EQ(x * a + y * b, std::gcd(a, b));
}

TEST(NumericTest, ExGCD) {
  for (int a = -10; a <= 100; ++a)
    for (int b = -100; b <= 10; ++b) {
      test_exgcd<int8_t>(a, b);
      test_exgcd<int16_t>(a, b);
      test_exgcd<int32_t>(a, b);
      test_exgcd<int64_t>(a, b);
    }
  // test large numbers
  std::default_random_engine rand_engine(
      testing::UnitTest::GetInstance()->random_seed());
  std::uniform_int_distribution<int64_t> rand_gen(
      std::numeric_limits<int64_t>::min(), std::numeric_limits<int64_t>::max());
  for (int i = 0; i < 1000; ++i) {
    int64_t a = rand_gen(rand_engine);
    int64_t b = rand_gen(rand_engine);
    test_exgcd(a, b);
  }
  // test overflow
  test_exgcd(std::numeric_limits<int>::min(), std::numeric_limits<int>::max());
  test_exgcd(std::numeric_limits<int>::max(), std::numeric_limits<int>::min());
  test_exgcd(std::numeric_limits<int>::max(), 1);
  test_exgcd(std::numeric_limits<int>::min(), 1);
  test_exgcd(1, std::numeric_limits<int>::max());
  test_exgcd(1, std::numeric_limits<int>::min());
}

}  // namespace number_theory
}  // namespace tql
