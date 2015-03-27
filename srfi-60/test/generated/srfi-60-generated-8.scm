(import (chibi) (chibi test) (srfi 60))

(test-begin "srfi-60/generated")

(test-begin "Bitwise Operations")

(test 0 (bitwise-and 136 55))
(test -255 (bitwise-and -117 -239))
(test 179 (bitwise-and 243 183))
(test -190 (bitwise-and -174 -61))
(test 20 (bitwise-and -170 28))
(test 55 (bitwise-and -201 255))
(test 17 (bitwise-and 57 -47))
(test 13 (bitwise-and 93 -211))
(test 183 (bitwise-and -73 247))
(test -256 (bitwise-and -31 -228))
(test -77 (bitwise-and -13 -65))
(test -256 (bitwise-and -253 -124))
(test 32 (bitwise-and -220 58))
(test 20 (bitwise-and 28 149))
(test 1 (bitwise-and 11 -203))
(test 0 (bitwise-and 36 147))
(test 0 (bitwise-and 6 -200))
(test -247 (bitwise-and -179 -117))
(test 72 (bitwise-and 75 88))
(test 137 (bitwise-and -7 141))
(test -152 (bitwise-and -129 -24))
(test -256 (bitwise-and -61 -224))
(test -188 (bitwise-and -60 -164))
(test 4 (bitwise-and 68 -75))
(test 82 (bitwise-and 250 -170))
(test -240 (bitwise-and -100 -208))
(test 4 (bitwise-and 182 12))
(test 1 (bitwise-and 165 11))
(test 0 (bitwise-and 17 -28))
(test 129 (bitwise-and 137 183))
(test 0 (bitwise-and 80 -88))
(test 152 (bitwise-and -72 218))
(test 160 (bitwise-and 225 174))
(test -248 (bitwise-and -216 -241))
(test 160 (bitwise-and 166 184))
(test 8 (bitwise-and 200 -216))
(test 2 (bitwise-and 210 -213))
(test 2 (bitwise-and -57 58))
(test 40 (bitwise-and 60 234))
(test 1 (bitwise-and -255 25))
(test 10 (bitwise-and -245 190))
(test 25 (bitwise-and 25 255))
(test 135 (bitwise-and 135 191))
(test 16 (bitwise-and 114 29))
(test 12 (bitwise-and -99 108))
(test 232 (bitwise-and -20 233))
(test 189 (bitwise-and 189 191))
(test -126 (bitwise-and -125 -18))
(test 18 (bitwise-and -73 26))
(test -176 (bitwise-and -164 -13))

(test 255 (bitwise-ior 188 219))
(test -168 (bitwise-ior -176 88))
(test -35 (bitwise-ior 77 -100))
(test 170 (bitwise-ior 170 8))
(test 253 (bitwise-ior 237 85))
(test -65 (bitwise-ior -98 179))
(test 255 (bitwise-ior 63 196))
(test -2 (bitwise-ior -22 86))
(test -196 (bitwise-ior 12 -208))
(test 253 (bitwise-ior 252 101))
(test -33 (bitwise-ior -164 -53))
(test -98 (bitwise-ior 158 -238))
(test 252 (bitwise-ior 188 76))
(test -1 (bitwise-ior -150 149))
(test 254 (bitwise-ior 246 108))
(test -147 (bitwise-ior 45 -156))
(test -131 (bitwise-ior -139 -147))
(test -1 (bitwise-ior 246 -147))
(test -129 (bitwise-ior 110 -131))
(test -58 (bitwise-ior 134 -188))
(test -2 (bitwise-ior -34 102))
(test -69 (bitwise-ior -104 -197))
(test -33 (bitwise-ior -34 -99))
(test 55 (bitwise-ior 23 38))
(test 253 (bitwise-ior 69 253))
(test -5 (bitwise-ior 171 -168))
(test -66 (bitwise-ior -198 190))
(test -69 (bitwise-ior 131 -72))
(test -1 (bitwise-ior -21 -131))
(test 127 (bitwise-ior 85 58))
(test -113 (bitwise-ior -247 143))
(test -1 (bitwise-ior -19 254))
(test -10 (bitwise-ior 242 -108))
(test 154 (bitwise-ior 18 136))
(test -1 (bitwise-ior -139 -50))
(test -69 (bitwise-ior -221 -70))
(test 111 (bitwise-ior 107 110))
(test -129 (bitwise-ior -229 116))
(test -105 (bitwise-ior 134 -237))
(test -1 (bitwise-ior -73 -149))
(test 159 (bitwise-ior 143 22))
(test -37 (bitwise-ior 217 -102))
(test -137 (bitwise-ior 34 -171))
(test 245 (bitwise-ior 69 177))
(test -35 (bitwise-ior -40 -179))
(test -89 (bitwise-ior -218 167))
(test -167 (bitwise-ior -247 -167))
(test 126 (bitwise-ior 118 72))
(test -3 (bitwise-ior 245 -8))
(test -73 (bitwise-ior -217 147))

(test -116 (bitwise-xor -190 206))
(test -15 (bitwise-xor 194 -205))
(test -19 (bitwise-xor 29 -16))
(test -173 (bitwise-xor 221 -114))
(test -190 (bitwise-xor 146 -48))
(test 112 (bitwise-xor -33 -81))
(test 83 (bitwise-xor -20 -65))
(test -248 (bitwise-xor 164 -84))
(test -210 (bitwise-xor 75 -155))
(test -240 (bitwise-xor 108 -132))
(test 208 (bitwise-xor 80 128))
(test 198 (bitwise-xor 47 233))
(test 2 (bitwise-xor 228 230))
(test -133 (bitwise-xor -252 127))
(test 95 (bitwise-xor -116 -45))
(test 44 (bitwise-xor -53 -25))
(test -60 (bitwise-xor -103 93))
(test -41 (bitwise-xor -156 179))
(test -97 (bitwise-xor 119 -24))
(test 42 (bitwise-xor 12 38))
(test 107 (bitwise-xor -69 -48))
(test 100 (bitwise-xor 46 74))
(test -42 (bitwise-xor 50 -28))
(test -152 (bitwise-xor 81 -199))
(test 35 (bitwise-xor 121 90))
(test -52 (bitwise-xor 84 -104))
(test 88 (bitwise-xor -228 -188))
(test -139 (bitwise-xor 56 -179))
(test -186 (bitwise-xor -73 241))
(test 73 (bitwise-xor -4 -75))
(test -181 (bitwise-xor 243 -72))
(test -101 (bitwise-xor 131 -232))
(test -251 (bitwise-xor 167 -94))
(test -226 (bitwise-xor 214 -56))
(test 185 (bitwise-xor 53 140))
(test -197 (bitwise-xor 74 -143))
(test -174 (bitwise-xor 200 -102))
(test 38 (bitwise-xor -38 -4))
(test 237 (bitwise-xor 1 236))
(test 73 (bitwise-xor 112 57))
(test -254 (bitwise-xor -74 180))
(test -90 (bitwise-xor -256 166))
(test -77 (bitwise-xor -172 231))
(test -171 (bitwise-xor -155 48))
(test -219 (bitwise-xor 226 -57))
(test -39 (bitwise-xor 32 -7))
(test -124 (bitwise-xor -55 77))
(test -128 (bitwise-xor -137 247))
(test -115 (bitwise-xor 152 -235))
(test -22 (bitwise-xor 44 -58))

(test 142 (bitwise-not -143))
(test -212 (bitwise-not 211))
(test -242 (bitwise-not 241))
(test -144 (bitwise-not 143))
(test -112 (bitwise-not 111))
(test -59 (bitwise-not 58))
(test -182 (bitwise-not 181))
(test -234 (bitwise-not 233))
(test 40 (bitwise-not -41))
(test -85 (bitwise-not 84))
(test 252 (bitwise-not -253))
(test -188 (bitwise-not 187))
(test 104 (bitwise-not -105))
(test -186 (bitwise-not 185))
(test 113 (bitwise-not -114))
(test -178 (bitwise-not 177))
(test -191 (bitwise-not 190))
(test 116 (bitwise-not -117))
(test -233 (bitwise-not 232))
(test -192 (bitwise-not 191))
(test -249 (bitwise-not 248))
(test 255 (bitwise-not -256))
(test -127 (bitwise-not 126))
(test -41 (bitwise-not 40))
(test 189 (bitwise-not -190))
(test -196 (bitwise-not 195))
(test -21 (bitwise-not 20))
(test -214 (bitwise-not 213))
(test -60 (bitwise-not 59))
(test -41 (bitwise-not 40))
(test 71 (bitwise-not -72))
(test 244 (bitwise-not -245))
(test 154 (bitwise-not -155))
(test 21 (bitwise-not -22))
(test -160 (bitwise-not 159))
(test 182 (bitwise-not -183))
(test -102 (bitwise-not 101))
(test -38 (bitwise-not 37))
(test 213 (bitwise-not -214))
(test -29 (bitwise-not 28))
(test 54 (bitwise-not -55))
(test 233 (bitwise-not -234))
(test 138 (bitwise-not -139))
(test -41 (bitwise-not 40))
(test 9 (bitwise-not -10))
(test -93 (bitwise-not 92))
(test -240 (bitwise-not 239))
(test 235 (bitwise-not -236))
(test 21 (bitwise-not -22))
(test 209 (bitwise-not -210))

(test 136 (bitwise-if 17 -94 153))
(test -10 (bitwise-if 65 72 -74))
(test -181 (bitwise-if 102 -165 -179))
(test 132 (bitwise-if 152 196 12))
(test 48 (bitwise-if -223 172 49))
(test -42 (bitwise-if 114 83 -44))
(test -28 (bitwise-if 217 -58 -148))
(test -180 (bitwise-if -225 -116 -168))
(test 85 (bitwise-if 73 97 29))
(test -192 (bitwise-if 100 -192 -156))
(test -33 (bitwise-if 121 93 -17))
(test 186 (bitwise-if -249 66 189))
(test -138 (bitwise-if -172 -132 -158))
(test -55 (bitwise-if -202 -255 201))
(test -155 (bitwise-if -54 -192 -217))
(test -3 (bitwise-if 60 -195 -43))
(test 63 (bitwise-if 13 -67 62))
(test -247 (bitwise-if -114 -136 -119))
(test -31 (bitwise-if -224 -96 193))
(test 190 (bitwise-if 180 182 46))
(test -247 (bitwise-if -206 -64 9))
(test 179 (bitwise-if 213 -69 163))
(test 151 (bitwise-if -225 87 143))
(test 220 (bitwise-if -16 209 236))
(test -120 (bitwise-if 34 -167 -118))
(test -8 (bitwise-if 175 -8 -38))
(test 249 (bitwise-if 145 -9 121))
(test 186 (bitwise-if 233 -84 186))
(test 147 (bitwise-if 195 -65 18))
(test 235 (bitwise-if 177 165 235))
(test -166 (bitwise-if -203 -232 79))
(test -163 (bitwise-if 62 -35 -177))
(test 198 (bitwise-if 78 246 128))
(test -160 (bitwise-if -25 -136 -93))
(test 213 (bitwise-if -164 247 -115))
(test 196 (bitwise-if -9 204 87))
(test 119 (bitwise-if 134 95 245))
(test 64 (bitwise-if -155 200 100))
(test -194 (bitwise-if 161 44 -226))
(test 115 (bitwise-if 54 243 81))
(test 251 (bitwise-if -237 63 -8))
(test -211 (bitwise-if 116 -209 -147))
(test -129 (bitwise-if -140 -139 11))
(test -182 (bitwise-if -214 -230 66))
(test -161 (bitwise-if -69 -225 111))
(test 171 (bitwise-if 97 -211 170))
(test -206 (bitwise-if -68 -141 14))
(test -76 (bitwise-if -17 -76 -67))
(test -169 (bitwise-if 119 -33 -234))
(test 137 (bitwise-if 244 139 57))

(test #t (any-bits-set? 55 -177))
(test #t (any-bits-set? -176 -133))
(test #t (any-bits-set? 215 68))
(test #t (any-bits-set? -127 223))
(test #t (any-bits-set? -157 39))
(test #t (any-bits-set? 28 -119))
(test #t (any-bits-set? -73 -229))
(test #t (any-bits-set? 72 -59))
(test #t (any-bits-set? -134 102))
(test #t (any-bits-set? -21 -204))
(test #t (any-bits-set? 95 -216))
(test #t (any-bits-set? -88 -76))
(test #t (any-bits-set? 22 221))
(test #t (any-bits-set? -196 17))
(test #t (any-bits-set? -28 -104))
(test #t (any-bits-set? -7 180))
(test #t (any-bits-set? 155 111))
(test #t (any-bits-set? 255 -21))
(test #t (any-bits-set? -123 150))
(test #t (any-bits-set? 61 83))
(test #t (any-bits-set? 250 -182))
(test #t (any-bits-set? 195 149))
(test #t (any-bits-set? -177 108))
(test #t (any-bits-set? 212 90))
(test #t (any-bits-set? -10 230))
(test #t (any-bits-set? -74 161))
(test #t (any-bits-set? -103 -46))
(test #t (any-bits-set? -167 -18))
(test #t (any-bits-set? -7 -119))
(test #t (any-bits-set? 123 140))
(test #t (any-bits-set? 22 -182))
(test #t (any-bits-set? 195 -111))
(test #t (any-bits-set? -210 -232))
(test #t (any-bits-set? 241 192))
(test #t (any-bits-set? -81 -76))
(test #t (any-bits-set? -3 217))
(test #t (any-bits-set? 133 163))
(test #t (any-bits-set? 15 -151))
(test #t (any-bits-set? 191 -145))
(test #t (any-bits-set? -107 163))
(test #t (any-bits-set? -20 -17))
(test #f (any-bits-set? 128 -219))
(test #t (any-bits-set? -203 -23))
(test #t (any-bits-set? 35 -45))
(test #t (any-bits-set? -89 166))
(test #t (any-bits-set? 220 -244))
(test #t (any-bits-set? 237 -145))
(test #t (any-bits-set? -47 211))
(test #f (any-bits-set? 172 -240))
(test #t (any-bits-set? 106 -219))

(test-end)

(test-begin "Integer Properties")

(test 7 (bit-count 127))
(test 2 (bit-count 10))
(test 3 (bit-count 19))
(test 4 (bit-count -84))
(test 2 (bit-count -21))
(test 2 (bit-count -66))
(test 3 (bit-count -135))
(test 7 (bit-count -192))
(test 5 (bit-count 171))
(test 4 (bit-count 108))
(test 5 (bit-count -183))
(test 7 (bit-count 247))
(test 4 (bit-count 141))
(test 5 (bit-count 103))
(test 4 (bit-count 30))
(test 5 (bit-count 213))
(test 5 (bit-count -122))
(test 3 (bit-count -134))
(test 3 (bit-count 41))
(test 4 (bit-count 150))
(test 7 (bit-count -255))
(test 3 (bit-count -68))
(test 6 (bit-count 235))
(test 3 (bit-count 162))
(test 3 (bit-count -36))
(test 5 (bit-count 173))
(test 5 (bit-count -186))
(test 5 (bit-count -174))
(test 3 (bit-count 49))
(test 5 (bit-count -187))
(test 2 (bit-count -13))
(test 7 (bit-count 254))
(test 3 (bit-count -23))
(test 2 (bit-count -66))
(test 6 (bit-count -208))
(test 2 (bit-count -10))
(test 5 (bit-count 211))
(test 4 (bit-count -143))
(test 2 (bit-count -11))
(test 3 (bit-count 168))
(test 4 (bit-count 141))
(test 1 (bit-count 16))
(test 5 (bit-count 229))
(test 4 (bit-count 120))
(test 3 (bit-count 176))
(test 5 (bit-count 167))
(test 5 (bit-count 234))
(test 3 (bit-count -177))
(test 2 (bit-count 160))
(test 2 (bit-count -13))

(test 8 (integer-length 140))
(test 6 (integer-length -55))
(test 8 (integer-length 200))
(test 8 (integer-length 243))
(test 8 (integer-length -144))
(test 7 (integer-length 118))
(test 8 (integer-length 243))
(test 6 (integer-length 55))
(test 7 (integer-length 120))
(test 6 (integer-length 41))
(test 5 (integer-length 23))
(test 7 (integer-length -87))
(test 7 (integer-length 95))
(test 8 (integer-length 129))
(test 8 (integer-length -252))
(test 6 (integer-length -53))
(test 8 (integer-length 134))
(test 8 (integer-length -202))
(test 8 (integer-length 243))
(test 8 (integer-length 173))
(test 8 (integer-length 235))
(test 8 (integer-length 176))
(test 8 (integer-length 172))
(test 8 (integer-length -253))
(test 7 (integer-length 83))
(test 8 (integer-length -220))
(test 7 (integer-length 85))
(test 5 (integer-length -19))
(test 7 (integer-length 103))
(test 5 (integer-length 19))
(test 8 (integer-length 241))
(test 7 (integer-length 74))
(test 6 (integer-length -48))
(test 2 (integer-length 2))
(test 8 (integer-length -205))
(test 8 (integer-length 143))
(test 6 (integer-length -60))
(test 8 (integer-length -146))
(test 8 (integer-length -231))
(test 8 (integer-length 235))
(test 7 (integer-length 94))
(test 8 (integer-length 248))
(test 8 (integer-length 135))
(test 8 (integer-length 158))
(test 8 (integer-length 192))
(test 5 (integer-length -23))
(test 6 (integer-length -40))
(test 6 (integer-length -40))
(test 7 (integer-length 81))
(test 3 (integer-length 4))

(test 1 (first-set-bit 178))
(test 2 (first-set-bit 44))
(test 1 (first-set-bit 106))
(test 1 (first-set-bit 226))
(test 0 (first-set-bit -137))
(test 0 (first-set-bit -103))
(test 0 (first-set-bit -217))
(test 5 (first-set-bit 160))
(test 0 (first-set-bit -67))
(test 2 (first-set-bit 36))
(test 1 (first-set-bit 70))
(test 4 (first-set-bit -48))
(test 1 (first-set-bit 142))
(test 2 (first-set-bit 36))
(test 0 (first-set-bit 103))
(test 0 (first-set-bit -79))
(test 0 (first-set-bit -227))
(test 0 (first-set-bit 235))
(test 0 (first-set-bit -61))
(test 1 (first-set-bit -166))
(test 0 (first-set-bit -41))
(test 3 (first-set-bit 232))
(test 2 (first-set-bit 132))
(test 0 (first-set-bit -221))
(test 0 (first-set-bit 9))
(test 0 (first-set-bit -215))
(test 2 (first-set-bit -156))
(test 0 (first-set-bit -137))
(test 2 (first-set-bit 76))
(test 2 (first-set-bit 52))
(test 0 (first-set-bit -33))
(test 3 (first-set-bit 232))
(test 0 (first-set-bit -151))
(test 1 (first-set-bit -106))
(test 3 (first-set-bit -56))
(test 0 (first-set-bit 127))
(test 0 (first-set-bit 67))
(test 1 (first-set-bit 186))
(test 2 (first-set-bit -60))
(test 0 (first-set-bit -251))
(test 1 (first-set-bit -206))
(test 2 (first-set-bit -204))
(test 0 (first-set-bit -181))
(test 1 (first-set-bit 170))
(test 4 (first-set-bit -144))
(test 1 (first-set-bit -66))
(test 1 (first-set-bit 66))
(test 1 (first-set-bit -58))
(test 2 (first-set-bit -188))
(test 6 (first-set-bit 192))

(test-end)

(test-begin "Bit Within Word")

(test #f (bit-set? 6 -126))
(test #f (bit-set? 7 84))
(test #t (bit-set? 1 142))
(test #t (bit-set? 0 151))
(test #f (bit-set? 7 -134))
(test #f (bit-set? 3 6))
(test #f (bit-set? 3 -28))
(test #t (bit-set? 5 -8))
(test #f (bit-set? 5 -117))
(test #f (bit-set? 7 -186))
(test #t (bit-set? 7 129))
(test #t (bit-set? 7 -53))
(test #t (bit-set? 3 -164))
(test #f (bit-set? 6 -224))
(test #f (bit-set? 6 -213))
(test #t (bit-set? 7 190))
(test #f (bit-set? 1 -163))
(test #f (bit-set? 6 173))
(test #f (bit-set? 0 16))
(test #f (bit-set? 7 -171))
(test #f (bit-set? 7 94))
(test #f (bit-set? 7 72))
(test #t (bit-set? 2 4))
(test #f (bit-set? 0 152))
(test #t (bit-set? 7 -80))
(test #f (bit-set? 7 -135))
(test #t (bit-set? 5 180))
(test #t (bit-set? 2 -34))
(test #f (bit-set? 1 176))
(test #t (bit-set? 6 -177))
(test #f (bit-set? 6 -225))
(test #t (bit-set? 4 211))
(test #f (bit-set? 4 172))
(test #f (bit-set? 0 -240))
(test #t (bit-set? 1 239))
(test #t (bit-set? 5 -212))
(test #t (bit-set? 0 -75))
(test #f (bit-set? 2 193))
(test #f (bit-set? 0 -194))
(test #f (bit-set? 7 51))
(test #t (bit-set? 4 146))
(test #f (bit-set? 2 -207))
(test #t (bit-set? 1 55))
(test #t (bit-set? 6 219))
(test #f (bit-set? 4 65))
(test #t (bit-set? 2 92))
(test #f (bit-set? 0 -82))
(test #f (bit-set? 5 0))
(test #t (bit-set? 5 96))
(test #t (bit-set? 3 -40))

(test 85 (copy-bit 0 85 #t))
(test -235 (copy-bit 3 -235 #f))
(test -238 (copy-bit 4 -254 #t))
(test 99 (copy-bit 4 99 #f))
(test 109 (copy-bit 6 45 #t))
(test -203 (copy-bit 3 -203 #f))
(test 2 (copy-bit 0 3 #f))
(test 33 (copy-bit 5 33 #t))
(test 245 (copy-bit 6 181 #t))
(test 181 (copy-bit 0 181 #t))
(test 11 (copy-bit 7 139 #f))
(test 256 (copy-bit 1 256 #f))
(test -247 (copy-bit 1 -247 #f))
(test 236 (copy-bit 5 236 #t))
(test 252 (copy-bit 4 236 #t))
(test -55 (copy-bit 0 -56 #t))
(test -233 (copy-bit 4 -249 #t))
(test 75 (copy-bit 1 75 #t))
(test -109 (copy-bit 5 -109 #f))
(test 47 (copy-bit 7 175 #f))
(test -38 (copy-bit 7 -166 #t))
(test 173 (copy-bit 2 169 #t))
(test -94 (copy-bit 7 -94 #t))
(test -90 (copy-bit 4 -74 #f))
(test 156 (copy-bit 6 156 #f))
(test -125 (copy-bit 2 -125 #f))
(test 128 (copy-bit 6 192 #f))
(test -150 (copy-bit 2 -150 #f))
(test -231 (copy-bit 7 -231 #f))
(test 139 (copy-bit 6 203 #f))
(test 99 (copy-bit 4 115 #f))
(test -107 (copy-bit 5 -75 #f))
(test -40 (copy-bit 3 -48 #t))
(test -162 (copy-bit 5 -130 #f))
(test -34 (copy-bit 2 -34 #t))
(test -129 (copy-bit 1 -131 #t))
(test -17 (copy-bit 6 -17 #t))
(test -228 (copy-bit 7 -228 #f))
(test -137 (copy-bit 3 -137 #f))
(test 18 (copy-bit 5 18 #f))
(test 239 (copy-bit 7 111 #t))
(test 115 (copy-bit 5 83 #t))
(test -54 (copy-bit 7 -182 #t))
(test 237 (copy-bit 5 237 #t))
(test -112 (copy-bit 3 -104 #f))
(test -200 (copy-bit 2 -196 #f))
(test 197 (copy-bit 0 197 #t))
(test 7 (copy-bit 1 7 #t))
(test -9 (copy-bit 5 -41 #t))
(test -161 (copy-bit 1 -163 #t))

(test-end)

(test-begin "Field of Bits")

(test 8 (bit-field -111 1 6))
(test 13 (bit-field 26 1 5))
(test 1 (bit-field -230 4 6))
(test 6 (bit-field -104 2 7))
(test 0 (bit-field 54 4 4))
(test 48 (bit-field -16 0 6))
(test 0 (bit-field -127 3 7))
(test 27 (bit-field 111 2 7))
(test 2 (bit-field -248 2 6))
(test 6 (bit-field -201 3 7))
(test 11 (bit-field 151 1 7))
(test 0 (bit-field -137 4 4))
(test 1 (bit-field -178 3 6))
(test 0 (bit-field -105 3 4))
(test 4 (bit-field -60 0 6))
(test 5 (bit-field -173 4 7))
(test 7 (bit-field 127 4 7))
(test 0 (bit-field -237 3 4))
(test 1 (bit-field -57 2 5))
(test 3 (bit-field 239 2 4))
(test 0 (bit-field -159 3 4))
(test 12 (bit-field 179 2 6))
(test 4 (bit-field -92 0 5))
(test 23 (bit-field 151 0 6))
(test 2 (bit-field -220 1 5))
(test 1 (bit-field -39 3 4))
(test 1 (bit-field -90 2 4))
(test 7 (bit-field -16 4 7))
(test 110 (bit-field 238 0 7))
(test 0 (bit-field 112 0 4))
(test 3 (bit-field -137 4 6))
(test 38 (bit-field -26 0 6))
(test 16 (bit-field 208 0 6))
(test 6 (bit-field 237 1 5))
(test 1 (bit-field 191 3 4))
(test 4 (bit-field -96 3 7))
(test 6 (bit-field -154 0 4))
(test 1 (bit-field -75 4 5))
(test 0 (bit-field 36 3 5))
(test 71 (bit-field 71 0 7))
(test 2 (bit-field -247 2 4))
(test 14 (bit-field -100 1 6))
(test 2 (bit-field -53 2 6))
(test 6 (bit-field -115 1 4))
(test 7 (bit-field -145 1 5))
(test 1 (bit-field -114 3 7))
(test 15 (bit-field -162 1 5))
(test 9 (bit-field -54 3 7))
(test 0 (bit-field -158 4 5))
(test 2 (bit-field -234 3 6))

(test -214 (copy-bit-field -166 -107 3 7))
(test 177 (copy-bit-field 161 -92 2 5))
(test -25 (copy-bit-field -22 -201 0 4))
(test -224 (copy-bit-field -220 -224 0 6))
(test 27 (copy-bit-field 27 161 4 5))
(test -212 (copy-bit-field -244 -254 4 7))
(test 115 (copy-bit-field 121 -71 1 5))
(test -170 (copy-bit-field -166 59 1 5))
(test 234 (copy-bit-field 234 -53 3 4))
(test 154 (copy-bit-field 176 141 1 7))
(test -70 (copy-bit-field -89 186 0 5))
(test 217 (copy-bit-field 161 219 3 7))
(test 146 (copy-bit-field 145 -190 0 4))
(test -80 (copy-bit-field -90 -168 1 6))
(test 171 (copy-bit-field 139 170 2 6))
(test 78 (copy-bit-field 65 -178 0 4))
(test -39 (copy-bit-field -7 25 0 6))
(test -47 (copy-bit-field -31 50 3 6))
(test 59 (copy-bit-field 3 -162 2 6))
(test -249 (copy-bit-field -217 -208 4 7))
(test -135 (copy-bit-field -131 -126 2 4))
(test -210 (copy-bit-field -194 -144 4 5))
(test -243 (copy-bit-field -243 -135 3 4))
(test 18 (copy-bit-field 82 65 4 7))
(test 179 (copy-bit-field 179 -142 3 5))
(test 241 (copy-bit-field 245 -4 2 5))
(test 45 (copy-bit-field 29 146 4 6))
(test -206 (copy-bit-field -206 -130 3 4))
(test -229 (copy-bit-field -229 190 2 5))
(test 102 (copy-bit-field 114 -71 2 7))
(test -241 (copy-bit-field -241 -176 4 6))
(test -10 (copy-bit-field -42 -45 4 6))
(test 211 (copy-bit-field 195 90 3 5))
(test 5 (copy-bit-field 53 -96 3 7))
(test 144 (copy-bit-field 240 -63 4 7))
(test -208 (copy-bit-field -224 -136 1 5))
(test -80 (copy-bit-field -80 -248 1 4))
(test -248 (copy-bit-field -256 -191 3 7))
(test -3 (copy-bit-field -23 -1 2 6))
(test 136 (copy-bit-field 136 86 4 5))
(test -34 (copy-bit-field -34 167 1 4))
(test 137 (copy-bit-field 129 -108 1 4))
(test 40 (copy-bit-field 40 230 4 4))
(test -48 (copy-bit-field -48 -216 2 4))
(test 107 (copy-bit-field 107 84 4 4))
(test -168 (copy-bit-field -168 -165 3 7))
(test 130 (copy-bit-field 158 -136 2 5))
(test 173 (copy-bit-field 141 133 3 6))
(test 46 (copy-bit-field 62 -174 4 7))
(test 66 (copy-bit-field 26 -168 3 7))

(test 3 (arithmetic-shift 198 -6))
(test -2 (arithmetic-shift -94 -6))
(test 0 (arithmetic-shift 198 -10))
(test -10 (arithmetic-shift -147 -4))
(test 29696 (arithmetic-shift 232 7))
(test 8704 (arithmetic-shift 68 7))
(test -1 (arithmetic-shift -1 0))
(test 3184 (arithmetic-shift 199 4))
(test 254 (arithmetic-shift 127 1))
(test -65 (arithmetic-shift -65 0))
(test -97 (arithmetic-shift -97 0))
(test -19584 (arithmetic-shift -153 7))
(test 2 (arithmetic-shift 151 -6))
(test -218112 (arithmetic-shift -213 10))
(test 10 (arithmetic-shift 168 -4))
(test -1008 (arithmetic-shift -126 3))
(test 5664 (arithmetic-shift 177 5))
(test 104 (arithmetic-shift 208 -1))
(test -3 (arithmetic-shift -12 -2))
(test -1 (arithmetic-shift -198 -10))
(test -1 (arithmetic-shift -12 -5))
(test -13824 (arithmetic-shift -27 9))
(test 140 (arithmetic-shift 35 2))
(test 0 (arithmetic-shift 12 -7))
(test -19968 (arithmetic-shift -39 9))
(test 5 (arithmetic-shift 44 -3))
(test -11392 (arithmetic-shift -178 6))
(test 7264 (arithmetic-shift 227 5))
(test -3 (arithmetic-shift -42 -4))
(test 192512 (arithmetic-shift 188 10))
(test 12800 (arithmetic-shift 100 7))
(test -1 (arithmetic-shift -184 -9))
(test -3 (arithmetic-shift -156 -6))
(test 14 (arithmetic-shift 232 -4))
(test -1384 (arithmetic-shift -173 3))
(test -24064 (arithmetic-shift -188 7))
(test -1 (arithmetic-shift -211 -10))
(test 0 (arithmetic-shift 233 -10))
(test 148 (arithmetic-shift 74 1))
(test 4 (arithmetic-shift 140 -5))
(test -146 (arithmetic-shift -146 0))
(test -286 (arithmetic-shift -143 1))
(test -27648 (arithmetic-shift -216 7))
(test -2 (arithmetic-shift -112 -6))
(test 0 (arithmetic-shift 71 -7))
(test -1296 (arithmetic-shift -162 3))
(test -1 (arithmetic-shift -145 -9))
(test -2 (arithmetic-shift -159 -7))
(test -61952 (arithmetic-shift -242 8))
(test 2864 (arithmetic-shift 179 4))

(test 96 (rotate-bit-field 48 -7 3 7))
(test -215 (rotate-bit-field -182 -10 0 7))
(test -12 (rotate-bit-field -12 -4 3 4))
(test -65 (rotate-bit-field -65 1 3 6))
(test -164 (rotate-bit-field -164 1 4 4))
(test -125 (rotate-bit-field -119 -8 1 7))
(test -141 (rotate-bit-field -141 1 4 6))
(test 23 (rotate-bit-field 27 3 1 6))
(test 245 (rotate-bit-field 245 0 4 6))
(test -113 (rotate-bit-field -101 -4 2 5))
(test 87 (rotate-bit-field 87 -4 4 6))
(test 112 (rotate-bit-field 112 -1 1 4))
(test -248 (rotate-bit-field -248 0 3 7))
(test 108 (rotate-bit-field 92 7 4 6))
(test 44 (rotate-bit-field 56 6 2 6))
(test -153 (rotate-bit-field -153 -6 4 5))
(test 225 (rotate-bit-field 225 -8 3 7))
(test -155 (rotate-bit-field -143 -10 1 5))
(test -239 (rotate-bit-field -191 -6 3 7))
(test -27 (rotate-bit-field -27 -4 3 4))
(test -157 (rotate-bit-field -157 -5 4 5))
(test 42 (rotate-bit-field 84 6 0 7))
(test -253 (rotate-bit-field -253 -3 2 4))
(test 15 (rotate-bit-field 15 0 0 5))
(test 144 (rotate-bit-field 132 2 0 5))
(test 26 (rotate-bit-field 28 -7 1 5))
(test -163 (rotate-bit-field -139 -3 2 7))
(test 208 (rotate-bit-field 208 -1 4 4))
(test 214 (rotate-bit-field 218 5 2 4))
(test -139 (rotate-bit-field -169 -8 1 7))
(test 41 (rotate-bit-field 49 7 1 5))
(test 137 (rotate-bit-field 137 0 0 7))
(test 148 (rotate-bit-field 152 -8 2 5))
(test -215 (rotate-bit-field -215 -3 3 6))
(test 64 (rotate-bit-field 64 9 0 6))
(test -11 (rotate-bit-field -11 -9 4 7))
(test 83 (rotate-bit-field 39 6 0 7))
(test 85 (rotate-bit-field 89 2 1 4))
(test -99 (rotate-bit-field -99 -3 2 4))
(test -3 (rotate-bit-field -5 -1 1 6))
(test -147 (rotate-bit-field -165 -4 1 7))
(test 216 (rotate-bit-field 212 -8 2 7))
(test 226 (rotate-bit-field 228 -1 1 5))
(test 234 (rotate-bit-field 214 -1 1 7))
(test -198 (rotate-bit-field -198 0 3 7))
(test 138 (rotate-bit-field 162 -10 1 7))
(test -17 (rotate-bit-field -3 -2 0 5))
(test -19 (rotate-bit-field -35 3 0 7))
(test 48 (rotate-bit-field 48 6 4 4))
(test -85 (rotate-bit-field -85 -4 4 6))

(test 118 (reverse-bit-field 55 0 7))
(test -244 (reverse-bit-field -232 1 6))
(test 137 (reverse-bit-field 161 3 6))
(test -223 (reverse-bit-field -251 2 6))
(test -232 (reverse-bit-field -232 3 4))
(test -78 (reverse-bit-field -78 4 4))
(test 46 (reverse-bit-field 58 2 5))
(test 140 (reverse-bit-field 131 0 4))
(test 38 (reverse-bit-field 50 2 5))
(test -63 (reverse-bit-field -48 0 5))
(test -155 (reverse-bit-field -155 4 4))
(test -76 (reverse-bit-field -84 3 5))
(test 206 (reverse-bit-field 242 2 6))
(test 253 (reverse-bit-field 253 2 5))
(test 125 (reverse-bit-field 125 3 4))
(test 96 (reverse-bit-field 96 1 5))
(test 80 (reverse-bit-field 66 1 5))
(test 41 (reverse-bit-field 35 1 4))
(test -240 (reverse-bit-field -240 3 4))
(test 250 (reverse-bit-field 238 2 5))
(test -123 (reverse-bit-field -123 3 5))
(test -83 (reverse-bit-field -89 1 4))
(test -229 (reverse-bit-field -233 1 5))
(test -180 (reverse-bit-field -206 1 7))
(test -125 (reverse-bit-field -125 3 7))
(test -93 (reverse-bit-field -117 3 6))
(test -190 (reverse-bit-field -190 4 4))
(test -86 (reverse-bit-field -90 2 4))
(test -91 (reverse-bit-field -91 2 6))
(test 43 (reverse-bit-field 43 1 4))
(test -38 (reverse-bit-field -20 1 6))
(test 246 (reverse-bit-field 246 1 6))
(test -163 (reverse-bit-field -163 1 6))
(test 83 (reverse-bit-field 92 0 4))
(test 52 (reverse-bit-field 44 1 7))
(test -155 (reverse-bit-field -155 3 4))
(test -62 (reverse-bit-field -110 4 7))
(test 203 (reverse-bit-field 211 1 7))
(test -28 (reverse-bit-field -24 2 4))
(test -202 (reverse-bit-field -202 2 5))
(test -93 (reverse-bit-field -87 1 4))
(test -168 (reverse-bit-field -168 4 5))
(test 154 (reverse-bit-field 150 0 6))
(test -248 (reverse-bit-field -254 0 5))
(test -236 (reverse-bit-field -176 2 7))
(test -152 (reverse-bit-field -152 2 5))
(test 79 (reverse-bit-field 103 2 7))
(test 120 (reverse-bit-field 71 0 6))
(test 65 (reverse-bit-field 80 0 5))
(test -250 (reverse-bit-field -232 1 5))

(test-end)

(test-begin "Bits as Booleans")

(test '(#f #f #f #t #f #f #f #t #f #f #t) (integer->list 137 11))
(test '(#f #f) (integer->list 4 2))
(test '(#f #t #t #t) (integer->list 39 4))
(test '(#f #t) (integer->list 177 2))
(test '() (integer->list 19 0))
(test '(#f #t #t #t #t #t #t #t) (integer->list 127 8))
(test '(#t) (integer->list 123 1))
(test '(#f #f #t #f #t #f #f) (integer->list 148 7))
(test '() (integer->list 115 0))
(test '(#t #f) (integer->list 94 2))
(test '(#t #t #t #t #f #f #t #t) (integer->list 243 8))
(test '(#f #f #f #t #f #t #t #t) (integer->list 23 8))
(test '(#f) (integer->list 254 1))
(test '(#t #t #f) (integer->list 6 3))
(test '(#f #f #t #t #t #t #t #t #f) (integer->list 126 9))
(test '(#f #t #f #t #t) (integer->list 235 5))
(test '(#t #t #t #f #t #t) (integer->list 59 6))
(test '(#f #f #f #t #t #f #t #f #t #t #t) (integer->list 215 11))
(test '(#t #f #f #f #f) (integer->list 176 5))
(test '(#t #t #f) (integer->list 246 3))
(test '(#f #f #t #f #f #f #t #f #f #t) (integer->list 137 10))
(test '() (integer->list 28 0))
(test '(#t #t #f #t #f #t #f) (integer->list 106 7))
(test '(#f #t #t #t #f #t) (integer->list 221 6))
(test '(#f #f #t #t #t #t #t #t #f #t) (integer->list 253 10))
(test '(#f #t) (integer->list 253 2))
(test '(#f #t #f #f #f) (integer->list 8 5))
(test '(#f #f #t #f #t #t #t #t) (integer->list 47 8))
(test '(#t #t #f #f) (integer->list 28 4))
(test '(#t #t #f #t #f) (integer->list 90 5))
(test '(#f #t) (integer->list 145 2))
(test '(#f #f #f #t #f #t #t) (integer->list 139 7))
(test '(#t #f #f #f #t) (integer->list 241 5))
(test '(#f #f #f #t #t #f #t #f #t) (integer->list 53 9))
(test '(#f) (integer->list 224 1))
(test '(#f #f #t) (integer->list 209 3))
(test '(#f #t #t #t #t #f #t) (integer->list 189 7))
(test '(#f #f #f #f) (integer->list 32 4))
(test '(#f #f) (integer->list 88 2))
(test '(#t #t #f #f #t #t) (integer->list 243 6))
(test '(#f #f) (integer->list 124 2))
(test '(#t #t #f) (integer->list 222 3))
(test '(#t) (integer->list 73 1))
(test '(#f #f #f #f #f #t #t #t #f #t #f) (integer->list 58 11))
(test '() (integer->list 104 0))
(test '(#f) (integer->list 76 1))
(test '(#f #t #t #t #f #f) (integer->list 156 6))
(test '(#f #t #f) (integer->list 170 3))
(test '(#t #t #t #t #f) (integer->list 30 5))
(test '(#f #t #f) (integer->list 34 3))

(test 7 (list->integer '(#t #t #t)))
(test 6 (list->integer '(#t #t #f)))
(test 5 (list->integer '(#f #f #f #t #f #t)))
(test 9 (list->integer '(#f #f #t #f #f #t)))
(test 2 (list->integer '(#t #f)))
(test 4 (list->integer '(#t #f #f)))
(test 5 (list->integer '(#f #t #f #t)))
(test 0 (list->integer '(#f #f #f)))
(test 9 (list->integer '(#t #f #f #t)))
(test 15 (list->integer '(#f #t #t #t #t)))
(test 2 (list->integer '(#t #f)))
(test 3 (list->integer '(#f #t #t)))
(test 6 (list->integer '(#t #t #f)))
(test 12 (list->integer '(#f #t #t #f #f)))
(test 0 (list->integer '(#f #f)))
(test 10 (list->integer '(#t #f #t #f)))
(test 3 (list->integer '(#f #t #t)))
(test 36 (list->integer '(#t #f #f #t #f #f)))
(test 0 (list->integer '()))
(test 22 (list->integer '(#t #f #t #t #f)))
(test 19 (list->integer '(#f #f #t #f #f #t #t)))
(test 0 (list->integer '(#f)))
(test 7 (list->integer '(#f #t #t #t)))
(test 1 (list->integer '(#f #f #t)))
(test 0 (list->integer '(#f #f)))
(test 0 (list->integer '(#f #f #f #f)))
(test 1 (list->integer '(#t)))
(test 0 (list->integer '()))
(test 64 (list->integer '(#t #f #f #f #f #f #f)))
(test 51 (list->integer '(#t #t #f #f #t #t)))
(test 8 (list->integer '(#f #t #f #f #f)))
(test 8 (list->integer '(#f #t #f #f #f)))
(test 37 (list->integer '(#t #f #f #t #f #t)))
(test 10 (list->integer '(#t #f #t #f)))
(test 18 (list->integer '(#f #f #t #f #f #t #f)))
(test 0 (list->integer '(#f #f)))
(test 44 (list->integer '(#t #f #t #t #f #f)))
(test 3 (list->integer '(#t #t)))
(test 0 (list->integer '(#f)))
(test 1 (list->integer '(#f #f #f #t)))
(test 0 (list->integer '()))
(test 31 (list->integer '(#t #t #t #t #t)))
(test 95 (list->integer '(#t #f #t #t #t #t #t)))
(test 0 (list->integer '(#f)))
(test 0 (list->integer '()))
(test 6 (list->integer '(#f #f #f #f #t #t #f)))
(test 6 (list->integer '(#f #f #t #t #f)))
(test 4 (list->integer '(#f #f #f #f #t #f #f)))
(test 15 (list->integer '(#f #f #t #t #t #t)))
(test 0 (list->integer '()))

(test-end)

(test-end)
