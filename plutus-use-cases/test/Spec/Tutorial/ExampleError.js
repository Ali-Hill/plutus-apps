*** Failed! Assertion failed (after 3 tests):                  
Actions 
 [Init (Wallet 5),
  NewLaw (Wallet 2) "lawv3"]
Expected funds of W[7] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken7",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken7",1)])])
Expected funds of W[8] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken8",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken8",1)])])
Expected funds of W[6] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken6",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken6",1)])])
Expected funds of W[4] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken4",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken4",1)])])
Expected funds of W[2] to change by
  Value (Map [(,Map [("",-2000000)])])
but they changed by
  Value (Map [(,Map [("",-20000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken2",1)])])
a discrepancy of
  Value (Map [(,Map [("",-18000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken2",1)])])
Expected funds of W[1] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken1",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken1",1)])])
Expected funds of W[10] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken10",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken10",1)])])
Expected funds of W[9] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken9",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken9",1)])])
Expected funds of W[3] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken3",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken3",1)])])
Expected funds of W[5] to change by
  Value (Map [])
but they changed by
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken5",1)])])
a discrepancy of
  Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken5",1)])])
Test failed.
Emulator log:
[INFO] Slot 0: TxnValidate 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6
[INFO] Slot 1: 00000000-0000-4000-8000-000000000000 {Wallet W[1]}:
                 Contract instance started
[INFO] Slot 1: 00000000-0000-4000-8000-000000000001 {Wallet W[2]}:
                 Contract instance started
[INFO] Slot 1: 00000000-0000-4000-8000-000000000002 {Wallet W[3]}:
                 Contract instance started
[INFO] Slot 1: 00000000-0000-4000-8000-000000000003 {Wallet W[4]}:
                 Contract instance started
[INFO] Slot 1: 00000000-0000-4000-8000-000000000004 {Wallet W[5]}:
                 Contract instance started
[INFO] Slot 1: 00000000-0000-4000-8000-000000000001 {Wallet W[2]}:
                 Receive endpoint call on 'new-law' for Object (fromList [("contents",Array [Object (fromList [("getEndpointDescription",String "new-law")]),Object (fromList [("unEndpointValue",String "6c61777633")])]),("tag",String "ExposeEndpointResp")])
[WARNING] Slot 1: 00000000-0000-4000-8000-000000000001 {Wallet W[2]}:
                    Contract log: String "Plutus.Contract.StateMachine.runInitialise: Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
[INFO] Slot 1: W[2]: Balancing an unbalanced transaction:
                       Tx:
                         Tx 42ec16af46d47268dfdbc461ecec94e88dd79fea6198f1210b8164ae5a412442:
                           {inputs:
                           collateral inputs:
                           outputs:
                             - Value (Map [(,Map [("",2000000)])]) addressed to
                               ScriptCredential: 3ac653d901ff3033db8a8c5dc9f2a706624b4681697a7336353370f6 (no staking credential)
                           mint: Value (Map [])
                           fee: Value (Map [])
                           mps:
                           signatures:
                           validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                           data:
                             <"lawv3",
                             "=\203\184\NAK_\192=\188@\232\169\190\224V\192\135\223\216[\229\250\&1\193\255\&7\171\206\218",
                             <>>}
                       Requires signatures:
                       Utxo index:
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!20
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!21
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!22
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!23
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!24
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!25
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!26
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!27
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!28
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                         ( 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!29
                         , - Value (Map [(,Map [("",10000000000000000)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential) )
                       Validity range:
                         (-? , +?)
[INFO] Slot 1: W[2]: Finished balancing:
                       Tx 6e72d68d3cdc046c34564779d2e8c221cd2f49f8b95b18a246d75235e043d8f2:
                         {inputs:
                            - 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!20

                         collateral inputs:
                           - 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!20

                         outputs:
                           - Value (Map [(,Map [("",9999999997999516)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                           - Value (Map [(,Map [("",2000000)])]) addressed to
                             ScriptCredential: 3ac653d901ff3033db8a8c5dc9f2a706624b4681697a7336353370f6 (no staking credential)
                         mint: Value (Map [])
                         fee: Value (Map [(,Map [("",484)])])
                         mps:
                         signatures:
                         validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                         data:
                           <"lawv3",
                           "=\203\184\NAK_\192=\188@\232\169\190\224V\192\135\223\216[\229\250\&1\193\255\&7\171\206\218",
                           <>>}
[INFO] Slot 1: W[2]: Signing tx: 6e72d68d3cdc046c34564779d2e8c221cd2f49f8b95b18a246d75235e043d8f2
[INFO] Slot 1: W[2]: Submitting tx: 6e72d68d3cdc046c34564779d2e8c221cd2f49f8b95b18a246d75235e043d8f2
[INFO] Slot 1: W[2]: TxSubmit: 6e72d68d3cdc046c34564779d2e8c221cd2f49f8b95b18a246d75235e043d8f2
[INFO] Slot 1: TxnValidate 6e72d68d3cdc046c34564779d2e8c221cd2f49f8b95b18a246d75235e043d8f2
[WARNING] Slot 2: 00000000-0000-4000-8000-000000000001 {Wallet W[2]}:
                    Contract log: String "Plutus.Contract.StateMachine.runStep: Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
[INFO] Slot 2: W[2]: Balancing an unbalanced transaction:
                       Tx:
                         Tx 2ef4816e597a3f6819973e8f160aadbfc58efb4e08badeb0dbfe715804486b64:
                           {inputs:
                              - 6e72d68d3cdc046c34564779d2e8c221cd2f49f8b95b18a246d75235e043d8f2!1
                                <["TestLawToken1",
                                "TestLawToken2",
                                "TestLawToken3",
                                "TestLawToken4",
                                "TestLawToken5",
                                "TestLawToken6",
                                "TestLawToken7",
                                "TestLawToken8",
                                "TestLawToken9",
                                "TestLawToken10"]>
                           collateral inputs:
                           outputs:
                             - Value (Map [(,Map [("",2000000)])]) addressed to
                               ScriptCredential: 3ac653d901ff3033db8a8c5dc9f2a706624b4681697a7336353370f6 (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken10",1)])]) addressed to
                               PubKeyCredential: a96a668ed7be83e332c872f51da7925b4472ca98c4f517efa4bbb9fb (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken9",1)])]) addressed to
                               PubKeyCredential: dfe12ac160d354b690385751a144e3010610fe5ecf5d0d266f5b6811 (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken8",1)])]) addressed to
                               PubKeyCredential: 8952ed1aff55f5b7674b122804a3c0a96f4e286352740c0cd0e9c746 (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken7",1)])]) addressed to
                               PubKeyCredential: c605888d3c40386d7c323a4679c767e5a0a7b683605c3e5df9a76aee (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken6",1)])]) addressed to
                               PubKeyCredential: 97add5c3ca491534a1d81165f637d338e072d47ec6af8100463c4a1d (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken5",1)])]) addressed to
                               PubKeyCredential: bf342ddd3b1a6191d4ce936c92d29834d6879edf2849eaea84c827f8 (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken4",1)])]) addressed to
                               PubKeyCredential: 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken3",1)])]) addressed to
                               PubKeyCredential: 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken2",1)])]) addressed to
                               PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                             - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken1",1)])]) addressed to
                               PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                           mint: Value (Map [(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken1",1),("TestLawToken10",1),("TestLawToken2",1),("TestLawToken3",1),("TestLawToken4",1),("TestLawToken5",1),("TestLawToken6",1),("TestLawToken7",1),("TestLawToken8",1),("TestLawToken9",1)])])
                           fee: Value (Map [])
                           mps:
                             MintingPolicy { <script> }
                           signatures:
                           validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                           data:
                             <"lawv3",
                             "=\203\184\NAK_\192=\188@\232\169\190\224V\192\135\223\216[\229\250\&1\193\255\&7\171\206\218",
                             <>>}
                       Requires signatures:
                       Utxo index:
                         ( 6e72d68d3cdc046c34564779d2e8c221cd2f49f8b95b18a246d75235e043d8f2!1
                         , - Value (Map [(,Map [("",2000000)])]) addressed to
                             ScriptCredential: 3ac653d901ff3033db8a8c5dc9f2a706624b4681697a7336353370f6 (no staking credential) )
                       Validity range:
                         (-? , +?)
[INFO] Slot 2: W[2]: Finished balancing:
                       Tx 38be367e5c72d9d6c331a5e25eb264ffb082dd52f23bc8bad2f2ea81aea3a71d:
                         {inputs:
                            - 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!21

                            - 6e72d68d3cdc046c34564779d2e8c221cd2f49f8b95b18a246d75235e043d8f2!1
                              <["TestLawToken1",
                              "TestLawToken2",
                              "TestLawToken3",
                              "TestLawToken4",
                              "TestLawToken5",
                              "TestLawToken6",
                              "TestLawToken7",
                              "TestLawToken8",
                              "TestLawToken9",
                              "TestLawToken10"]>
                         collateral inputs:
                           - 331170b4130e90cf688d52a395fe1a712c305f343587547d47ea1dd8071e17a6!21

                         outputs:
                           - Value (Map [(,Map [("",9999999978243231)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                           - Value (Map [(,Map [("",2000000)])]) addressed to
                             ScriptCredential: 3ac653d901ff3033db8a8c5dc9f2a706624b4681697a7336353370f6 (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken10",1)])]) addressed to
                             PubKeyCredential: a96a668ed7be83e332c872f51da7925b4472ca98c4f517efa4bbb9fb (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken9",1)])]) addressed to
                             PubKeyCredential: dfe12ac160d354b690385751a144e3010610fe5ecf5d0d266f5b6811 (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken8",1)])]) addressed to
                             PubKeyCredential: 8952ed1aff55f5b7674b122804a3c0a96f4e286352740c0cd0e9c746 (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken7",1)])]) addressed to
                             PubKeyCredential: c605888d3c40386d7c323a4679c767e5a0a7b683605c3e5df9a76aee (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken6",1)])]) addressed to
                             PubKeyCredential: 97add5c3ca491534a1d81165f637d338e072d47ec6af8100463c4a1d (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken5",1)])]) addressed to
                             PubKeyCredential: bf342ddd3b1a6191d4ce936c92d29834d6879edf2849eaea84c827f8 (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken4",1)])]) addressed to
                             PubKeyCredential: 557d23c0a533b4d295ac2dc14b783a7efc293bc23ede88a6fefd203d (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken3",1)])]) addressed to
                             PubKeyCredential: 2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken2",1)])]) addressed to
                             PubKeyCredential: 80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7 (no staking credential)
                           - Value (Map [(,Map [("",2000000)]),(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken1",1)])]) addressed to
                             PubKeyCredential: a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2 (no staking credential)
                         mint: Value (Map [(3dcbb8155fc03dbc40e8a9bee056c087dfd85be5fa31c1ff37abceda,Map [("TestLawToken1",1),("TestLawToken10",1),("TestLawToken2",1),("TestLawToken3",1),("TestLawToken4",1),("TestLawToken5",1),("TestLawToken6",1),("TestLawToken7",1),("TestLawToken8",1),("TestLawToken9",1)])])
                         fee: Value (Map [(,Map [("",1756769)])])
                         mps:
                           MintingPolicy { <script> }
                         signatures:
                         validity range: Interval {ivFrom = LowerBound NegInf True, ivTo = UpperBound PosInf True}
                         data:
                           <"lawv3",
                           "=\203\184\NAK_\192=\188@\232\169\190\224V\192\135\223\216[\229\250\&1\193\255\&7\171\206\218",
                           <>>}
[INFO] Slot 2: W[2]: Signing tx: 38be367e5c72d9d6c331a5e25eb264ffb082dd52f23bc8bad2f2ea81aea3a71d
[INFO] Slot 2: W[2]: Submitting tx: 38be367e5c72d9d6c331a5e25eb264ffb082dd52f23bc8bad2f2ea81aea3a71d
[INFO] Slot 2: W[2]: TxSubmit: 38be367e5c72d9d6c331a5e25eb264ffb082dd52f23bc8bad2f2ea81aea3a71d
[INFO] Slot 2: 00000000-0000-4000-8000-000000000005 {supercalifragilisticexpialidocious}:
                 Contract instance started
[INFO] Slot 2: 00000000-0000-4000-8000-000000000005 {supercalifragilisticexpialidocious}:
                 Receive endpoint call on 'register-token-env' for Object (fromList [("contents",Array [Object (fromList [("getEndpointDescription",String "register-token-env")]),Object (fromList [("unEndpointValue",Object (fromList [("0",Object (fromList [])),("1",Object (fromList []))]))])]),("tag",String "ExposeEndpointResp")])
[INFO] Slot 2: 00000000-0000-4000-8000-000000000005 {supercalifragilisticexpialidocious}:
                 Contract instance stopped (no errors)
