sed -n -e'1p' frm.hdr > E1STDFRM.TXT.A
sed -n -e'2p' frm.hdr > E1STDFRM.TXT.B
sed -n -e'3p' frm.hdr > E1STDFRM.TXT.C
sed -n -e'4p' frm.hdr > E1STDFRM.TXT.D
sed -n -e'5p' frm.hdr > E1STDFRM.TXT.E
sed -n -e'6p' frm.hdr > E1STDFRM.TXT.F
sed -n -e'7p' frm.hdr > E1STDFRM.TXT.G
sed -n -e'8p' frm.hdr > E2STDFRM.TXT
sed -n -e'9p' frm.hdr > J1STDFRM.TXT

mko0 data/E1A >> E1STDFRM.TXT.A
mko0 data/E1B >> E1STDFRM.TXT.B
mko0 data/E1C >> E1STDFRM.TXT.C
mko0 data/E1D >> E1STDFRM.TXT.D
mko0 data/E1E >> E1STDFRM.TXT.E
mko0 data/E1F | ./sublon.pl 96.18 >> E1STDFRM.TXT.F
mko0 data/E1G >> E1STDFRM.TXT.G

cat E1STDFRM.TXT.[A-G] > E1STDFRM.TXT
rm E1STDFRM.TXT.[A-G]

mko0 data/E2A >> E2STDFRM.TXT
mko0 data/J1A >> J1STDFRM.TXT
