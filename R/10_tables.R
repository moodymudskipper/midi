# tabr::guitarChords

#datapasta::tribble_paste()

# channel_voices_messages0 <-tibble::tribble(
#   ~Event,  ~Status,   ~Description,
#   "1000nnnn", "sent when a note is released (ended).",
#   "1001nnnn", "sent when a note is depressed (start).",
#   "1010nnnn", "most often sent by pressing down on the key after it \"bottoms out\".",
#   "1011nnnn", "sent when a controller value changes. Controllers include devices such as pedals and levers. Certain controller numbers are reserved for specific purposes. See Channel Mode Messages.",
#   "1100nnnn", "sent when the patch number changes.",
#   "1101nnnn", "most often sent by pressing down on the key after it \"bottoms out\". Different from polyphonic after-touch. Use to send the single greatest pressure value (of all the current depressed keys).",
#   "1110nnnn", "sent to indicate a change in the pitch wheel. The pitch wheel is measured by a fourteen bit value. Centre (no pitch change) is 2000H. Sensitivity is a function of the transmitter."
# )
#
# channel_voices_messages <-tibble::tribble(
#   ~Event,     ~quartet1,                            ~Description,     ~`Data Byte(s)`,                                           ~byte_descr,
#   "1000nnnn",       "8",                       "Note Off event.", "0kkkkkkk 0vvvvvvv",                        "key (note) number. velocity.",
#   "1001nnnn",       "9",                        "Note On event.", "0kkkkkkk 0vvvvvvv",                        "key (note) number. velocity.",
#   "1010nnnn",       "a", "Polyphonic Key Pressure (Aftertouch).", "0kkkkkkk 0vvvvvvv",                  "key (note) number. pressure value.",
#   "1011nnnn",       "b",                       "Control Change.", "0ccccccc 0vvvvvvv",                       "controller number. new value.",
#   "1100nnnn",       "c",                       "Program Change.",          "0ppppppp",                                 "new program number.",
#   "1101nnnn",       "d",       "Channel Pressure (After-touch).",          "0vvvvvvv",                                     "pressure value.",
#   "1110nnnn",       "e",                   "Pitch Wheel Change.", "0lllllll 0mmmmmmm",  "least significant 7 bits. most significant 7 bits."
# )

channel_voices_messages <-tibble::tribble(
   ~nibble1,               ~event,                ~byte_descr,
        "8",           "Note Off",               "key_number",
        "8",           "Note Off",                 "velocity",
        "9",            "Note On",               "key_number",
        "9",            "Note On",                 "velocity",
        "a",    "Note Aftertouch",               "key_number",
        "a",    "Note Aftertouch",           "pressure_value",
  # below commented should be treated separately
        "b",     "Control Change",        "controller_number",
        "b",     "Control Change",                "new_value",
        "c",     "Program Change",       "new_program_number",
        "d", "Channel Aftertouch",           "pressure_value",
        "e",         "Pitch Bend", "least_significant_7_bits",
        "e",         "Pitch Bend",  "most_significant_7_bits"
)

controller_messages <- "byte2_bin\tbyte2_hex\tbyte2_dec\tdescr\tbyte3_descr\tUse
00000000	00	0	Bank Select	0-127	MSB
00000001	01	1	* Modulation wheel	0-127	MSB
00000010	02	2	Breath control	0-127	MSB
00000011	03	3	Undefined	0-127	MSB
00000100	04	4	Foot controller	0-127	MSB
00000101	05	5	Portamento time	0-127	MSB
00000110	06	6	Data Entry	0-127	MSB
00000111	07	7	* Channel Volume (formerly Main Volume)	0-127	MSB
00001000	08	8	Balance	0-127	MSB
00001001	09	9	Undefined	0-127	MSB
00001010	0A	10	* Pan	0-127	MSB
00001011	0B	11	* Expression Controller	0-127	MSB
00001100	0C	12	Effect control 1	0-127	MSB
00001101	0D	13	Effect control 2	0-127	MSB
00001110	0E	14	Undefined	0-127	MSB
00001111	0F	15	Undefined	0-127	MSB
00010000	10	16	General Purpose Controller #1	0-127	MSB
00010001	11	17	General Purpose Controller #2	0-127	MSB
00010010	12	18	General Purpose Controller #3	0-127	MSB
00010011	13	19	General Purpose Controller #4	0-127	MSB
00010100	14	20	Undefined	0-127	MSB
00010101	15	21	Undefined	0-127	MSB
00010110	16	22	Undefined	0-127	MSB
00010111	17	23	Undefined	0-127	MSB
00011000	18	24	Undefined	0-127	MSB
00011001	19	25	Undefined	0-127	MSB
00011010	1A	26	Undefined	0-127	MSB
00011011	1B	27	Undefined	0-127	MSB
00011100	1C	28	Undefined	0-127	MSB
00011101	1D	29	Undefined	0-127	MSB
00011110	1E	30	Undefined	0-127	MSB
00011111	1F	31	Undefined	0-127	MSB
00100000	20	32	Bank Select	0-127	LSB
00100001	21	33	Modulation wheel	0-127	LSB
00100010	22	34	Breath control	0-127	LSB
00100011	23	35	Undefined	0-127	LSB
00100100	24	36	Foot controller	0-127	LSB
00100101	25	37	Portamento time	0-127	LSB
00100110	26	38	Data entry	0-127	LSB
00100111	27	39	Channel Volume (formerly Main Volume)	0-127	LSB
00101000	28	40	Balance	0-127	LSB
00101001	29	41	Undefined	0-127	LSB
00101010	2A	42	Pan	0-127	LSB
00101011	2B	43	Expression Controller	0-127	LSB
00101100	2C	44	Effect control 1	0-127	LSB
00101101	2D	45	Effect control 2	0-127	LSB
00101110	2E	46	Undefined	0-127	LSB
00101111	2F	47	Undefined	0-127	LSB
00110000	30	48	General Purpose Controller #1	0-127	LSB
00110001	31	49	General Purpose Controller #2	0-127	LSB
00110010	32	50	General Purpose Controller #3	0-127	LSB
00110011	33	51	General Purpose Controller #4	0-127	LSB
00110100	34	52	Undefined	0-127	LSB
00110101	35	53	Undefined	0-127	LSB
00110110	36	54	Undefined	0-127	LSB
00110111	37	55	Undefined	0-127	LSB
00111000	38	56	Undefined	0-127	LSB
00111001	39	57	Undefined	0-127	LSB
00111010	3A	58	Undefined	0-127	LSB
00111011	3B	59	Undefined	0-127	LSB
00111100	3C	60	Undefined	0-127	LSB
00111101	3D	61	Undefined	0-127	LSB
00111110	3E	62	Undefined	0-127	LSB
00111111	3F	63	Undefined	0-127	LSB
01000000	40	64	* Damper pedal on/off (Sustain)	<63=off	>64=on
01000001	41	65	Portamento on/off	<63=off >64=on\t
01000010	42	66	Sustenuto on/off	<63=off >64=on\t
01000011	43	67	Soft pedal on/off	<63=off >64=on\t
01000100	44	68	Legato Footswitch	<63=off >64=on\t
01000101	45	69	Hold 2	<63=off	>64=on
01000110	46	70	Sound Controller 1 (Sound Variation)	0-127	LSB
01000111	47	71	Sound Controller 2 (Timbre)	0-127	LSB
01001000	48	72	Sound Controller 3 (Release Time)	0-127	LSB
01001001	49	73	Sound Controller 4 (Attack Time)	0-127	LSB
01001010	4A	74	Sound Controller 5 (Brightness)	0-127	LSB
01001011	4B	75	Sound Controller 6	0-127	LSB
01001100	4C	76	Sound Controller 7	0-127	LSB
01001101	4D	77	Sound Controller 8	0-127	LSB
01001110	4E	78	Sound Controller 9	0-127	LSB
01001111	4F	79	Sound Controller 10	0-127	LSB
01010000	50	80	General Purpose Controller #5	0-127	LSB
01010001	51	81	General Purpose Controller #6	0-127	LSB
01010010	52	82	General Purpose Controller #7	0-127	LSB
01010011	53	83	General Purpose Controller #8	0-127	LSB
01010100	54	84	Portamento Control	0-127	Source Note
01010101	55	85	Undefined	0-127	LSB
01010110	56	86	Undefined	0-127	LSB
01010111	57	87	Undefined	0-127	LSB
01011000	58	88	Undefined	0-127	LSB
01011001	59	89	Undefined	0-127	LSB
01011010	5A	90	Undefined	0-127	LSB
01011011	5B	91	Effects 1 Depth	0-127	LSB
01011100	5C	92	Effects 2 Depth	0-127	LSB
01011101	5D	93	Effects 3 Depth	0-127	LSB
01011110	5E	94	Effects 4 Depth	0-127	LSB
01011111	5F	95	Effects 5 Depth	0-127	LSB
01100000	60	96	Data entry +1	N/A\t
01100001	61	97	Data entry -1	N/A\t
01100010	62	98	Non-Registered Parameter Number LSB	0-127	LSB
01100011	63	99	Non-Registered Parameter Number MSB	0-127	MSB
01100100	64	100	* Registered Parameter Number LSB	0-127	LSB
01100101	65	101	* Registered Parameter Number MSB	0-127	MSB
01100110	66	102	Undefined	?\t
01100111	67	103	Undefined	?\t
01101000	68	104	Undefined	?\t
01101001	69	105	Undefined	?\t
01101010	6A	106	Undefined	?\t
01101011	6B	107	Undefined	?\t
01101100	6C	108	Undefined	?\t
01101101	6D	109	Undefined	?\t
01101110	6E	110	Undefined	?\t
01101111	6F	111	Undefined	?\t
01110000	70	112	Undefined	?\t
01110001	71	113	Undefined	?\t
01110010	72	114	Undefined	?\t
01110011	73	115	Undefined	?\t
01110100	74	116	Undefined	?\t
01110101	75	117	Undefined	?\t
01110110	76	118	Undefined	?\t
01110111	77	119	Undefined	?\t
01111000	78	120	All Sound Off	0\t
01111001	79	121	* Reset All Controllers	0\t
01111010	7A	122	Local control on/off	0=off 127=on\t
01111011	7B	123	* All notes off	0\t
01111100	7C	124	Omni mode off (+ all notes off)	0\t
01111101	7D	125	Omni mode on (+ all notes off)	0\t
01111110	7E	126	Poly mode on/off (+ all notes off)	**\t
01111111	7F	127	Poly mode on (incl mono=off +all notes off)	0\t"
controller_messages <- read.table(text= controller_messages, header=TRUE, sep="\t", comment.char = "")
controller_messages$descr <- sub("^\\* ", "", controller_messages$descr)
controller_messages$descr <- sub(" (formerly Main Volume)", "", controller_messages$descr, fixed=TRUE)

key_numbers <- "octave 	C	C#	D	D#	E	F	F#	G	G#	A	A#	B
-1	0	1	2	3	4	5	6	7	8	9	10	11
0	12	13	14	15	16	17	18	19	20	21	22	23
1	24	25	26	27	28	29	30	31	32	33	34	35
2	36	37	38	39	40	41	42	43	44	45	46	47
3	48	49	50	51	52	53	54	55	56	57	58	59
4	60	61	62	63	64	65	66	67	68	69	70	71
5	72	73	74	75	76	77	78	79	80	81	82	83
6	84	85	86	87	88	89	90	91	92	93	94	95
7	96	97	98	99	100	101	102	103	104	105	106	107
8	108	109	110	111	112	113	114	115	116	117	118	119
9	120	121	122	123	124	125	126	127\t\t\t\t
"
key_numbers <- read.table(text= key_numbers, header=TRUE, sep="\t", comment.char = "")
key_numbers <-tidyr::gather(key_numbers, note, key_number, -1)
key_numbers <- subset(key_numbers, !is.na(key_number))
key_numbers <- key_numbers[order(key_numbers$key_number),]
key_numbers$note <- sub(".", "#", key_numbers$note, fixed = TRUE)
key_numbers <- dplyr::mutate(key_numbers, note2 = paste0(note, octave, " (", key_number, ")"))



# special values of c for control change

# c = 122, v = 0: Local Control Off
# c = 122, v = 127: Local Control On
# c = 123, v = 0: All Notes Off
# c = 124, v = 0: Omni Mode Off
# c = 125, v = 0: Omni Mode On
# c = 126, v = M: Mono Mode On (Poly Off) where M is the number of channels (Omni Off) or 0 (Omni On)
# c = 127, v = 0: Poly Mode On (Mono Off) (Note: These four messages also cause All Notes Off)


# tibble::tribble(
#   ~`Delta-Time`,     ~Event,                             ~Comments,
#   "00",    "C2 46",                                    NA,
#   "00", "92 30 60",                                    NA,
#   "00",    "3C 60",                      "running status",
#   "83 00",    "30 00", "two-byte delta-time, running status",
#   "00",    "3C 00",                      "running status",
#   "00", "FF 2F 00",                        "end of track"
# )
