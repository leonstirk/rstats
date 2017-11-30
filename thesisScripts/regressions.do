// regressions

xtreg marocmv 
reg marocv if year == 2006
reg marocv if year == 2013

//xtreg rocmv
xtreg rocmv dlnqv dlnqf sstt inurptt fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother lninc ed0inc ed1inc ed2inc ed3inc ineteuinc inetmainc inetppinc inetasinc inetotherinc inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf, fe

reg rocmv dlnqv dlnqf sausqkmln sstt inurptt saudistcbd dt2p dtot dtnf fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p inetma inetpp inetas inetother lninc ineteuinc inetmainc inetppinc inetasinc inetotherinc inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2001
reg rocmv dlnqv dlnqf sausqkmln sstt inurptt saudistcbd dt2p dtot dtnf fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother lninc ed0inc ed1inc ed2inc ed3inc ineteuinc inetmainc inetppinc inetasinc inetotherinc inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2006
reg rocmv dlnqv dlnqf sausqkmln sstt inurptt saudistcbd dt2p dtot dtnf fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother lninc ed0inc ed1inc ed2inc ed3inc ineteuinc inetmainc inetppinc inetasinc inetotherinc inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2013

xtreg lnmv dlnqv dlnqf sstt inurptt fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother lninc ed0inc ed1inc ed2inc ed3inc ineteuinc inetmainc inetppinc inetasinc inetotherinc inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf, fe

reg lnmv dlnqv dlnqf sausqkmln sstt inurptt saudistcbd fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p inetma inetpp inetas inetother lninc ineteuinc inetmainc inetppinc inetasinc inetotherinc inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2001
reg lnmv dlnqv dlnqf sausqkmln sstt inurptt saudistcbd fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother lninc ed0inc ed1inc ed2inc ed3inc ineteuinc inetmainc inetppinc inetasinc inetotherinc inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2006
reg lnmv dlnqv dlnqf sausqkmln sstt inurptt saudistcbd fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother lninc ed0inc ed1inc ed2inc ed3inc ineteuinc inetmainc inetppinc inetasinc inetotherinc inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2013

xtreg lninc dlnqv dlnqf sstt inurptt fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf, fe

reg lninc dlnqv dlnqf sausqkmln sstt inurptt saudistcbd fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15l inab15t64 inab65p inetma inetpp inetas inetother inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2001
reg lninc dlnqv dlnqf sausqkmln sstt inurptt saudistcbd fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15l inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2006
reg lninc dlnqv dlnqf sausqkmln sstt inurptt saudistcbd fmpdcc fmpdsp hhcm2f hhcm3fp hhcmom hhcmop hhcmnf inab15l inab15t64 inab65p ed1 ed2 ed3 inetma inetpp inetas inetother inbpos inbpnf inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf if year == 2013

reg dmv2001t2013 i.qnt2000


//diversity
dlnqv dlnqf

//density and distance
sausqkmln sstt inurptt saudistcbd countAssesments

//dwelling type
dt2p dtot dtnf

//family type
fmpdcc fmpdsp

//household type
hhcm2f hhcm3fp hhcmom hhcmop hhcmnf

// no of children
inchl1 inchl2 inchl3 inchl4 inchl5 inchl6 inchlob inchlnf

//age
inab15l inab15t64 inab65p

//education
ed1 ed2 ed3 

//ethnicity
inetma inetpp inetas inetother

//income and income interactions
lninc ed0inc ed1inc ed2inc ed3inc ineteuinc inetmainc inetppinc inetasinc inetotherinc

//migrant and length of time in nz
inbpnf inosysa0t9a inosysa10t19a inosysa20t29a inosysa30t39a inosysa40t49a inosysa50pa inosysanfa

//state housing
hhslgovta hhslnfa

//resident flow
inyur1t4 inyur5t9 inyur10t14 inyur15t29 inyur30p inyurnf

//transport
inmtwhm inmtwdng inmtwcom inmtwpas inmtwbus inmtwtrn inmtwmcy inmtwbcy inmtwwlk inmtwot inmtwnf



