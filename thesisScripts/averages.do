egen avmv2000 = mean(rocmv) if year==2000 & _clus_4 == 4
egen avmv2001 = mean(rocmv) if year==2001 & _clus_4 == 4
egen avmv2002 = mean(rocmv) if year==2002 & _clus_4 == 4
egen avmv2003 = mean(rocmv) if year==2003 & _clus_4 == 4
egen avmv2004 = mean(rocmv) if year==2004 & _clus_4 == 4
egen avmv2005 = mean(rocmv) if year==2005 & _clus_4 == 4
egen avmv2006 = mean(rocmv) if year==2006 & _clus_4 == 4
egen avmv2007 = mean(rocmv) if year==2007 & _clus_4 == 4
egen avmv2008 = mean(rocmv) if year==2008 & _clus_4 == 4
egen avmv2009 = mean(rocmv) if year==2009 & _clus_4 == 4
egen avmv2010 = mean(rocmv) if year==2010 & _clus_4 == 4
egen avmv2011 = mean(rocmv) if year==2011 & _clus_4 == 4
egen avmv2012 = mean(rocmv) if year==2012 & _clus_4 == 4
egen avmv2013 = mean(rocmv) if year==2013 & _clus_4 == 4
egen avmv2014 = mean(rocmv) if year==2014 & _clus_4 == 4
egen avmv2015 = mean(rocmv) if year==2015 & _clus_4 == 4
egen avmv2016 = mean(rocmv) if year==2016 & _clus_4 == 4

foreach x of varlist avmv2000 avmv2001 avmv2002 avmv2003 avmv2004 avmv2005 avmv2006 avmv2007 avmv2008 avmv2009 avmv2010 avmv2011 avmv2012 avmv2013 avmv2014 avmv2015 avmv2016 {
replace `x' = 0 if `x' ==.
}

gen avclus3 = avmv2000 + avmv2001 + avmv2002 + avmv2003 + avmv2004 + avmv2005 +avmv2006 + avmv2007 + avmv2008 + avmv2009 + avmv2010 +avmv2011 + avmv2011 + avmv2012 +avmv2013 +avmv2014 +avmv2015 +avmv2016

drop avmv20*

replace avclus1 = . if avclus1 == 0 & year == 2000
