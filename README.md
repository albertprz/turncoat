# turncoat

A minimalistic bitboard based UCI chess engine written in Haskell with a strong focus on a clear and modular implementation with adequate performance when compared with other open source chess engines. 

Turncoat has an official account on Lichess: https://lichess.org/@/TurncoatEngine


## Characteristics

### Move Generation

- Legal Move Generation
- Specialized Static Exchanges Move Generator
- Staged Move Generation (captures & promotions, quiet moves)


### Search

- Negamax
- Alpha Beta Pruning
- Principal Variation Search
- Quiescence Search
- Iterative Deepening
- Transposition Table
- Null Move Pruning
- Futility Pruning
- Check Extensions
- Single move extensions
- Late Move Reductions


### Move Ordering

- Staged Move Ordering (via lazy lists)
- Static Exchange Evaluation
- Killer Moves
- Static Evaluation Ordering (for quiet moves)


### Evaluation

- Hand Tuned Evaluation
- Tapered Evaluation
- Material
- Piece Square Tables
- King Safety
- Mobility
- Passed Pawns
- Pawn King Shield
- Isolated & Doubled Pawns
- Knight Outposts
- Bishop Pair
- Rooks on Open Files
- Piece Threats


