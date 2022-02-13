# Add all tests
set(SOURCES
  numeric_unittest.cpp
  modular_unittest.cpp
  sieve_unittest.cpp
  # ... more
)
add_unittest(${SOURCES})

# Expose to parent scope 
expose_unittest()
