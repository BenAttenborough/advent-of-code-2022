module Data.Advent12Data exposing (difficultInput, pruningInput, realInput, simplifiedInput, testInput)


testInput : String
testInput =
    """Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi"""


pruningInput : String
pruningInput =
    """Sa
ab
"""


difficultInput : String
difficultInput =
    """
Sabcccc
ccccccc
ccccccc
ccccccc
ccccccc
"""


realInput : String
realInput =
    """abacccaaaacccccccccccaaaaaacccccaaaaaaccccaaacccccccccccccccccccccccccccccccccccccccccccaaaaa
abaaccaaaacccccccccccaaaaaaccccccaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccaaaaa
abaaccaaaacccccccccccaaaaacccccaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccaaaaa
abccccccccccccccccccccaaaaacccaaaaaaaaaaaaaaaacccccccccccccccccccccccccccaaaccccccccccccaaaaa
abccccccccccccccccccccaacaacccaaaaaaaaccaaaaaccccccccccccccccccccccccccccaaaccccccccccccaccaa
abcccccccccccccaacccaaaccccccaaaaaaaaaccaaaaaccccccccccccccccccccccccccccccacccccccccccccccca
abcccccccccccaaaaaaccaaaccacccccaaaaaaacccccccccccccccccccccccccciiiicccccccddddddccccccccccc
abcccccccccccaaaaaaccaaaaaaaccccaaaaaacccccaacccccccaaaccccccccciiiiiiiicccdddddddddacaaccccc
abccccccccccccaaaaaaaaaaaaacccccaaaaaaacaaaacccccccaaaacccccccchhiiiiiiiiicddddddddddaaaccccc
abcccccccccccaaaaaaaaaaaaaacccccccaaacccaaaaaacccccaaaaccccccchhhipppppiiiijjjjjjjddddaaccccc
abcccccccccccaaaaaaaaaaaaaaccccccccccccccaaaaaccccccaaaccccccchhhpppppppiijjjjjjjjjddeeaccccc
abcccccccccccccccccaaaaaaaacccccccccccccaaaaaccccccccccccccccchhppppppppppjjqqqjjjjjeeeaacccc
abccccccccccccccccccaaaaaaaacccccccccccccccaacccccccccccccccchhhpppuuuupppqqqqqqqjjjeeeaacccc
abcccccccccccccccccccaacccacccccccccccccccccccccccccccccccccchhhopuuuuuuppqqqqqqqjjjeeecccccc
abacccccccccccccaaacaaaccccccccccccccccccccccccccccaaccccccchhhhoouuuuuuuqvvvvvqqqjkeeecccccc
abaccccccccccccaaaaaacccccaaccccccccccccccccccccccaaaccccccchhhooouuuxxxuvvvvvvqqqkkeeecccccc
abaccccccccccccaaaaaacccaaaaaaccccccccccccccccccaaaaaaaaccchhhhooouuxxxxuvyyyvvqqqkkeeecccccc
abcccccccccccccaaaaacccaaaaaaaccccccccccccccccccaaaaaaaaccjjhooooouuxxxxyyyyyvvqqqkkeeecccccc
abccccccccccccccaaaaaacaaaaaaaccccccccaaaccccccccaaaaaaccjjjooootuuuxxxxyyyyyvvqqkkkeeecccccc
abccccccccccccccaaaaaaaaaaaaacccccccccaaaacccccccaaaaaacjjjooootttuxxxxxyyyyvvrrrkkkeeecccccc
SbccccccccccccccccccaaaaaaaaacccccccccaaaacccccccaaaaaacjjjoootttxxxEzzzzyyvvvrrrkkkfffcccccc
abcccccccccccaaacccccaaaaaaacaaaccccccaaaccccccccaaccaacjjjoootttxxxxxyyyyyyvvvrrkkkfffcccccc
abcccccccccaaaaaacccaaaaaacccaaacacccaacccccccccccccccccjjjoootttxxxxyxyyyyyywvvrrkkkfffccccc
abcccccccccaaaaaacccaaaaaaaaaaaaaaaccaaacaaacccccaacccccjjjnnnttttxxxxyyyyyyywwwrrkkkfffccccc
abcaacacccccaaaaacccaaacaaaaaaaaaaaccaaaaaaacccccaacaaacjjjnnnntttttxxyywwwwwwwwrrrlkfffccccc
abcaaaaccccaaaaacccccccccaacaaaaaaccccaaaaaacccccaaaaacccjjjnnnnnttttwwywwwwwwwrrrrllfffccccc
abaaaaaccccaaaaaccccccaaaaaccaaaaacaaaaaaaaccccaaaaaaccccjjjjinnnntttwwwwwsssrrrrrllllffccccc
abaaaaaaccccccccccccccaaaaacaaaaaacaaaaaaaaacccaaaaaaacccciiiiinnnntswwwwssssrrrrrlllfffccccc
abacaaaaccccccccccccccaaaaaacaaccccaaaaaaaaaaccccaaaaaaccccciiiinnnssswwsssssllllllllfffccccc
abccaaccccccccccccccccaaaaaaccccccccccaaacaaaccccaaccaacccccciiiinnsssssssmmllllllllfffaacccc
abccccccccccccccccccccaaaaaaccccccccccaaaccccccccaaccccccccccciiinnmsssssmmmmlllllgggffaacccc
abcccccccccccccccaccccccaaacccccccccccaaccccccccccccccccccccccciiimmmsssmmmmmgggggggggaaacccc
abcccccccccaaaaaaaaccccccccccccccccccccccccccccaaaaaccccccccccciiimmmmmmmmmgggggggggaaacccccc
abccccccccccaaaaaaccccccccccccccccccaacccccccccaaaaacccccccccccciiimmmmmmmhhggggcaaaaaaaccccc
abccccccccccaaaaaacccccccccccccccccaacccccccccaaaaaacccccccccccciihhmmmmhhhhgccccccccaacccccc
abccccaacaaaaaaaaaaccccccccccccccccaaaccccccccaaaaaaccccccccccccchhhhhhhhhhhaaccccccccccccccc
abccccaaaaaaaaaaaaaaccccccccccaaccaaaaccccccccaaaaaacccaaacccccccchhhhhhhhaaaaccccccccccccccc
abcccaaaaaaaaaaaaaaaccccccccaaaaaacaaaacacaccccaaaccccaaaacccccccccchhhhccccaaccccccccccaaaca
abcccaaaaaacacaaacccccccccccaaaaaaaaaaaaaaacccccccccccaaaacccccccccccaaaccccccccccccccccaaaaa
abcccccaaaacccaaaccccccccccaaaaaaaaaaaaaaaaccccccccccccaaacccccccccccaaacccccccccccccccccaaaa
abcccccaacccccaacccccccccccaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccccccccaaaaa"""


simplifiedInput : String
simplifiedInput =
    """abacccaaaacccccccccccaaaaaacccccaaaaaaccccaaacccccccccccccccccccccccccccccccccccccccccccaaaaa
abaaccaaaacccccccccccaaaaaaccccccaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccaaaaa
abaaccaaaacccccccccccaaaaacccccaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccaaaaa
abccccccccccccccccccccaaaaacccaaaaaaaaaaaaaaaacccccccccccccccccccccccccccaaaccccccccccccaaaaa
abccccccccccccccccccccaacaacccaaaaaaaaccaaaaaccccccccccccccccccccccccccccaaaccccccccccccaccaa
abcccccccccccccaacccaaaccccccaaaaaaaaaccaaaaaccccccccccccccccccccccccccccccacccccccccccccccca
abcccccccccccaaaaaaccaaaccacccccaaaaaaacccccccccccccccccccccccccciiiicccccccddddddccccccccccc
abcccccccccccaaaaaaccaaaaaaaccccaaaaaacccccaacccccccaaaccccccccciiiiiiiicccdddddddddacaaccccc
abccccccccccccaaaaaaaaaaaaacccccaaaaaaacaaaacccccccaaaacccccccchhiiiiiiiiicddddddddddaaaccccc
abcccccccccccaaaaaaaaaaaaaacccccccaaacccaaaaaacccccaaaaccccccchhhipppppiiiijjjjjjjddddaaccccc
abcccccccccccaaaaaaaaaaaaaaccccccccccccccaaaaaccccccaaaccccccchhhpppppppiijjjjjjjjjddeeaccccc
abcccccccccccccccccaaaaaaaacccccccccccccaaaaaccccccccccccccccchhppppppppppjjqqqjjjjjeeeaacccc
abccccccccccccccccccaaaaaaaacccccccccccccccaacccccccccccccccchhhpppuuuupppqqqqqqqjjjeeeaacccc
abcccccccccccccccccccaacccacccccccccccccccccccccccccccccccccchhhopuuuuuuppqqqqqqqjjjeeecccccc
abacccccccccccccaaacaaaccccccccccccccccccccccccccccaaccccccchhhhoouuuuuuuqvvvvvqqqjkeeecccccc
abaccccccccccccaaaaaacccccaaccccccccccccccccccccccaaaccccccchhhooouuuxxxuvvvvvvqqqkkeeecccccc
abaccccccccccccaaaaaacccaaaaaaccccccccccccccccccaaaaaaaaccchhhhooouuxxxxuvyyyvvqqqkkeeecccccc
abcccccccccccccaaaaacccaaaaaaaccccccccccccccccccaaaaaaaaccjjhooooouuxxxxyyyyyvvqqqkkeeecccccc
abccccccccccccccaaaaaacaaaaaaaccccccccaaaccccccccaaaaaaccjjjooootuuuxxxxyyyyyvvqqkkkeeecccccc
abccccccccccccccaaaaaaaaaaaaacccccccccaaaacccccccaaaaaacjjjooootttuxxxxxyyyyvvrrrkkkeeecccccc
SabcdefghijklmnopqrstuvwxyzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzEzzzzyyvvvrrrkkkfffcccccc
abcccccccccccaaacccccaaaaaaacaaaccccccaaaccccccccaaccaacjjjoootttxxxxxyyyyyyvvvrrkkkfffcccccc
abcccccccccaaaaaacccaaaaaacccaaacacccaacccccccccccccccccjjjoootttxxxxyxyyyyyywvvrrkkkfffccccc
abcccccccccaaaaaacccaaaaaaaaaaaaaaaccaaacaaacccccaacccccjjjnnnttttxxxxyyyyyyywwwrrkkkfffccccc
abcaacacccccaaaaacccaaacaaaaaaaaaaaccaaaaaaacccccaacaaacjjjnnnntttttxxyywwwwwwwwrrrlkfffccccc
abcaaaaccccaaaaacccccccccaacaaaaaaccccaaaaaacccccaaaaacccjjjnnnnnttttwwywwwwwwwrrrrllfffccccc
abaaaaaccccaaaaaccccccaaaaaccaaaaacaaaaaaaaccccaaaaaaccccjjjjinnnntttwwwwwsssrrrrrllllffccccc
abaaaaaaccccccccccccccaaaaacaaaaaacaaaaaaaaacccaaaaaaacccciiiiinnnntswwwwssssrrrrrlllfffccccc
abacaaaaccccccccccccccaaaaaacaaccccaaaaaaaaaaccccaaaaaaccccciiiinnnssswwsssssllllllllfffccccc
abccaaccccccccccccccccaaaaaaccccccccccaaacaaaccccaaccaacccccciiiinnsssssssmmllllllllfffaacccc
abccccccccccccccccccccaaaaaaccccccccccaaaccccccccaaccccccccccciiinnmsssssmmmmlllllgggffaacccc
abcccccccccccccccaccccccaaacccccccccccaaccccccccccccccccccccccciiimmmsssmmmmmgggggggggaaacccc
abcccccccccaaaaaaaaccccccccccccccccccccccccccccaaaaaccccccccccciiimmmmmmmmmgggggggggaaacccccc
abccccccccccaaaaaaccccccccccccccccccaacccccccccaaaaacccccccccccciiimmmmmmmhhggggcaaaaaaaccccc
abccccccccccaaaaaacccccccccccccccccaacccccccccaaaaaacccccccccccciihhmmmmhhhhgccccccccaacccccc
abccccaacaaaaaaaaaaccccccccccccccccaaaccccccccaaaaaaccccccccccccchhhhhhhhhhhaaccccccccccccccc
abccccaaaaaaaaaaaaaaccccccccccaaccaaaaccccccccaaaaaacccaaacccccccchhhhhhhhaaaaccccccccccccccc
abcccaaaaaaaaaaaaaaaccccccccaaaaaacaaaacacaccccaaaccccaaaacccccccccchhhhccccaaccccccccccaaaca
abcccaaaaaacacaaacccccccccccaaaaaaaaaaaaaaacccccccccccaaaacccccccccccaaaccccccccccccccccaaaaa
abcccccaaaacccaaaccccccccccaaaaaaaaaaaaaaaaccccccccccccaaacccccccccccaaacccccccccccccccccaaaa
abcccccaacccccaacccccccccccaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccccccccccccaaaaa"""
