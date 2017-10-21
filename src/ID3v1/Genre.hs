{-# LANGUAGE LambdaCase #-}

module ID3v1.Genre
    ( toGenre
    , fromGenre
    ) where

import Data.Word (Word8)
import qualified Data.ByteString.Char8 as C

toGenre :: Word8 -> Maybe String
toGenre = \case
    0   -> Just "Blues"
    1   -> Just "Classic Rock"
    2   -> Just "Country"
    3   -> Just "Dance"
    4   -> Just "Disco"
    5   -> Just "Funk"
    6   -> Just "Grunge"
    7   -> Just "Hip-Hop"
    8   -> Just "Jazz"
    9   -> Just "Metal"
    10  -> Just "New Age"
    11  -> Just "Oldies"
    12  -> Just "Other"
    13  -> Just "Pop"
    14  -> Just "R&B"
    15  -> Just "Rap"
    16  -> Just "Reggae"
    17  -> Just "Rock"
    18  -> Just "Techno"
    19  -> Just "Industrial"
    20  -> Just "Alternative"
    21  -> Just "Ska"
    22  -> Just "Death Metal"
    23  -> Just "Pranks"
    24  -> Just "Soundtrack"
    25  -> Just "Euro-Techno"
    26  -> Just "Ambient"
    27  -> Just "Trip-Hop"
    28  -> Just "Vocal"
    29  -> Just "Jazz+Funk"
    30  -> Just "Fusion"
    31  -> Just "Trance"
    32  -> Just "Classical"
    33  -> Just "Instrumental"
    34  -> Just "Acid"
    35  -> Just "House"
    36  -> Just "Game"
    37  -> Just "Sound Clip"
    38  -> Just "Gospel"
    39  -> Just "Noise"
    40  -> Just "AlternRock"
    41  -> Just "Bass"
    42  -> Just "Soul"
    43  -> Just "Punk"
    44  -> Just "Space"
    45  -> Just "Meditative"
    46  -> Just "Instrumental Pop"
    47  -> Just "Instrumental Rock"
    48  -> Just "Ethnic"
    49  -> Just "Gothic"
    50  -> Just "Darkwave"
    51  -> Just "Techno-Industrial"
    52  -> Just "Electronic"
    53  -> Just "Pop-Folk"
    54  -> Just "Eurodance"
    55  -> Just "Dream"
    56  -> Just "Southern Rock"
    57  -> Just "Comedy"
    58  -> Just "Cult"
    59  -> Just "Gangsta"
    60  -> Just "Top 40"
    61  -> Just "Christian Rap"
    62  -> Just "Pop/Funk"
    63  -> Just "Jungle"
    64  -> Just "Native American"
    65  -> Just "Cabaret"
    66  -> Just "New Wave"
    67  -> Just "Psychedelic"
    68  -> Just "Rave"
    69  -> Just "Showtunes"
    70  -> Just "Trailer"
    71  -> Just "Lo-Fi"
    72  -> Just "Tribal"
    73  -> Just "Acid Punk"
    74  -> Just "Acid Jazz"
    75  -> Just "Polka"
    76  -> Just "Retro"
    77  -> Just "Musical"
    78  -> Just "Rock & Roll"
    79  -> Just "Hard Rock"
    80  -> Just "Folk"
    81  -> Just "Folk-Rock"
    82  -> Just "National Folk"
    83  -> Just "Swing"
    84  -> Just "Fast Fusion"
    85  -> Just "Bebob"
    86  -> Just "Latin"
    87  -> Just "Revival"
    88  -> Just "Celtic"
    89  -> Just "Bluegrass"
    90  -> Just "Avantgarde"
    91  -> Just "Gothic Rock"
    92  -> Just "Progressive Rock"
    93  -> Just "Psychedelic Rock"
    94  -> Just "Symphonic Rock"
    95  -> Just "Slow Rock"
    96  -> Just "Big Band"
    97  -> Just "Chorus"
    98  -> Just "Easy Listening"
    99  -> Just "Acoustic"
    100 -> Just "Humour"
    101 -> Just "Speech"
    102 -> Just "Chanson"
    103 -> Just "Opera"
    104 -> Just "Chamber Music"
    105 -> Just "Sonata"
    106 -> Just "Symphony"
    107 -> Just "Booty Bass"
    108 -> Just "Primus"
    109 -> Just "Porn Groove"
    110 -> Just "Satire"
    111 -> Just "Slow Jam"
    112 -> Just "Club"
    113 -> Just "Tango"
    114 -> Just "Samba"
    115 -> Just "Folklore"
    116 -> Just "Ballad"
    117 -> Just "Power Ballad"
    118 -> Just "Rhythmic Soul"
    119 -> Just "Freestyle"
    120 -> Just "Duet"
    121 -> Just "Punk Rock"
    122 -> Just "Drum Solo"
    123 -> Just "A capella"
    124 -> Just "Euro-House"
    125 -> Just "Dance Hall"
    126 -> Just "Goa"
    127 -> Just "Drum & Bass"
    128 -> Just "Club-House"
    129 -> Just "Hardcore"
    130 -> Just "Terror"
    131 -> Just "Indie"
    132 -> Just "Britpop"
    133 -> Just "Negerpunk"
    134 -> Just "Polsk Punk"
    135 -> Just "Beat"
    136 -> Just "Christian Gangsta Rap"
    137 -> Just "Heavy Metal"
    138 -> Just "Black Metal"
    139 -> Just "Crossover"
    140 -> Just "Contemporary Christian"
    141 -> Just "Christian Rock "
    142 -> Just "Merengue"
    143 -> Just "Salsa"
    144 -> Just "Thrash Metal"
    145 -> Just "Anime"
    146 -> Just "JPop"
    147 -> Just "Synthpop"
    _   -> Nothing
    
fromGenre :: String -> Word8
fromGenre = \case
    "Blues"                  -> 0
    "Classic Rock"           -> 1
    "Country"                -> 2
    "Dance"                  -> 3 
    "Disco"                  -> 4
    "Funk"                   -> 5
    "Grunge"                 -> 6
    "Hip-Hop"                -> 7
    "Jazz"                   -> 8
    "Metal"                  -> 9
    "New Age"                -> 10
    "Oldies"                 -> 11
    "Other"                  -> 12
    "Pop"                    -> 13
    "R&B"                    -> 14
    "Rap"                    -> 15
    "Reggae"                 -> 16
    "Rock"                   -> 17
    "Techno"                 -> 18
    "Industrial"             -> 19
    "Alternative"            -> 20
    "Ska"                    -> 21
    "Death Metal"            -> 22
    "Pranks"                 -> 23
    "Soundtrack"             -> 24
    "Euro-Techno"            -> 25
    "Ambient"                -> 26
    "Trip-Hop"               -> 27
    "Vocal"                  -> 28
    "Jazz+Funk"              -> 29
    "Fusion"                 -> 30
    "Trance"                 -> 31
    "Classical"              -> 32
    "Instrumental"           -> 33
    "Acid"                   -> 34
    "House"                  -> 35
    "Game"                   -> 36
    "Sound Clip"             -> 37
    "Gospel"                 -> 38
    "Noise"                  -> 39
    "AlternRock"             -> 40
    "Bass"                   -> 41
    "Soul"                   -> 42
    "Punk"                   -> 43
    "Space"                  -> 44
    "Meditative"             -> 45
    "Instrumental Pop"       -> 46
    "Instrumental Rock"      -> 47
    "Ethnic"                 -> 48
    "Gothic"                 -> 49
    "Darkwave"               -> 50
    "Techno-Industrial"      -> 51
    "Electronic"             -> 52
    "Pop-Folk"               -> 53
    "Eurodance"              -> 54
    "Dream"                  -> 55
    "Southern Rock"          -> 56
    "Comedy"                 -> 57
    "Cult"                   -> 58
    "Gangsta"                -> 59
    "Top 40"                 -> 60
    "Christian Rap"          -> 61
    "Pop/Funk"               -> 62
    "Jungle"                 -> 63
    "Native American"        -> 64
    "Cabaret"                -> 65
    "New Wave"               -> 66
    "Psychedelic"            -> 67
    "Rave"                   -> 68
    "Showtunes"              -> 69
    "Trailer"                -> 70
    "Lo-Fi"                  -> 71
    "Tribal"                 -> 72
    "Acid Punk"              -> 73
    "Acid Jazz"              -> 74
    "Polka"                  -> 75
    "Retro"                  -> 76
    "Musical"                -> 77
    "Rock & Roll"            -> 78
    "Hard Rock"              -> 79
    "Folk"                   -> 80
    "Folk-Rock"              -> 81
    "National Folk"          -> 82
    "Swing"                  -> 83
    "Fast Fusion"            -> 84
    "Bebob"                  -> 85
    "Latin"                  -> 86
    "Revival"                -> 87
    "Celtic"                 -> 88
    "Bluegrass"              -> 89
    "Avantgarde"             -> 90
    "Gothic Rock"            -> 91
    "Progressive Rock"       -> 92
    "Psychedelic Rock"       -> 93
    "Symphonic Rock"         -> 94
    "Slow Rock"              -> 95
    "Big Band"               -> 96
    "Chorus"                 -> 97
    "Easy Listening"         -> 98
    "Acoustic"               -> 99
    "Humour"                 -> 100
    "Speech"                 -> 101
    "Chanson"                -> 102
    "Opera"                  -> 103
    "Chamber Music"          -> 104
    "Sonata"                 -> 105
    "Symphony"               -> 106
    "Booty Bass"             -> 107
    "Primus"                 -> 108
    "Porn Groove"            -> 109
    "Satire"                 -> 110
    "Slow Jam"               -> 111
    "Club"                   -> 112
    "Tango"                  -> 113
    "Samba"                  -> 114
    "Folklore"               -> 115
    "Ballad"                 -> 116
    "Power Ballad"           -> 117
    "Rhythmic Soul"          -> 118
    "Freestyle"              -> 119
    "Duet"                   -> 120
    "Punk Rock"              -> 121
    "Drum Solo"              -> 122
    "A capella"              -> 123
    "Euro-House"             -> 124
    "Dance Hall"             -> 125
    "Goa"                    -> 126
    "Drum & Bass"            -> 127
    "Club-House"             -> 128
    "Hardcore"               -> 129
    "Terror"                 -> 130
    "Indie"                  -> 131
    "Britpop"                -> 132
    "Negerpunk"              -> 133
    "Polsk Punk"             -> 134
    "Beat"                   -> 135
    "Christian Gangsta Rap"  -> 136
    "Heavy Metal"            -> 137
    "Black Metal"            -> 138
    "Crossover"              -> 139
    "Contemporary Christian" -> 140
    "Christian Rock "        -> 141
    "Merengue"               -> 142
    "Salsa"                  -> 143
    "Thrash Metal"           -> 144
    "Anime"                  -> 145
    "JPop"                   -> 146
    "Synthpop"               -> 147
