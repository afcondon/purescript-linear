module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Linear.Additive (runAdditiveTests)
import Test.Linear.Metric (runMetricTests)
import Test.Linear.Quaternion (runQuaternionTests)

main :: Effect Unit
main = do
  log "========================================"
  log "  purescript-linear property tests"
  log "========================================"
  log ""

  log "=== Additive typeclass ==="
  runAdditiveTests
  log ""

  log "=== Metric typeclass ==="
  runMetricTests
  log ""

  log "=== Quaternion operations ==="
  runQuaternionTests
  log ""

  log "========================================"
  log "  All tests completed!"
  log "========================================"
