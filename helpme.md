diff --git a/plutus-use-cases/src/Plutus/Contracts/Tutorial/Spec.hs b/plutus-use-cases/src/Plutus/Contracts/Tutorial/Spec.hs
index 8697c642a..2e0dcf461 100644
--- a/plutus-use-cases/src/Plutus/Contracts/Tutorial/Spec.hs
+++ b/plutus-use-cases/src/Plutus/Contracts/Tutorial/Spec.hs
@@ -391,8 +391,13 @@ prop_UnitTest = withMaxSuccess 1 $ forAllDL unitTest2 prop_Escrow
 -- | Certification.
 certification :: Certification EscrowModel
 certification = defaultCertification {
+    certNoLockedFunds = Just noLockProof,
+    certCrashTolerance = Just Instance,
+    certUnitTests = Just unitTest,
+    certDLTests = [("redeem test", unitTest1), ("refund test", unitTest2)],
     certCoverageIndex      = covIdx
   }
+  where unitTest _ = tests
 
 check_propEscrowWithCoverage :: IO ()
 check_propEscrowWithCoverage = do
