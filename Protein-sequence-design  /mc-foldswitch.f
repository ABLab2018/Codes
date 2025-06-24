	parameter(mcseq=1,mcstep=10000,npro=4931,nupro=4930,nmut=1)
	parameter(nsite=56,na=20,nf=10,nbin=200,np=47111,np1=167923)
	parameter(np2=150563,np3=119053,np4=253038,np5=315127)
	parameter(np6=367068,np7=307860,np8=352986,np9=360735)
	parameter(np10=371114,np12=47120,np13=167929,np14=150558)
	parameter(np15=119055,np16=253027,np17=315110,np18=367097)
	parameter(np19=307854,np20=352987,np21=360773,np22=371145)
	
	real cc,cc1,cc2,eold(npro,nsite),eold1(npro),random,kT1,kT2,kT3
	real ccc,cc4,cc5,eavunew2,simn1,simnew,p,p1,p2,p3,pfinal
	double precision eun1,eun2,eunew1,eunew2
	integer ssite1(nmut),site1,aseq(nsite),aseq1(nsite),arand(nmut)
	integer ssite2(nmut),site2,aseqn(nsite),aseqn1(nsite),countstep
	real oneold,oneold1,oneold2(npro,nsite),en1,en2,eold2(npro)
	real oned,oned1,oned2(npro,nsite),ss11,ss22,simcut,simcut1
	real onenew,onenew1,onenew2(npro,nsite),sim1,sim2,simold
	real onen,onen1,onen2(npro,nsite),enw1,enw2,enew1(npro)
	real told,told1,told2(npro,nsite),eavun1,eavun2,delold1,delold2
	real told3,told11,told22(npro,nsite),td12,td1,td11(npro,nsite)
	real told4,told31,told32(npro,nsite),td13,td2,td22(npro,nsite)
	real told5,told51,told52(npro,nsite),td14,td3,td33(npro,nsite)
	real told6,told61,told62(npro,nsite),td15,td4,td44(npro,nsite)
	real told7,told71,told72(npro,nsite),td16,td5,td55(npro,nsite)
	real told8,told81,told82(npro,nsite),td17,td6,td66(npro,nsite)
	real told9,told91,told92(npro,nsite),td18,td7,td77(npro,nsite)
	real told13,told131,told132(npro,nsite),told10,told101
	real told12,told121,told122(npro,nsite),dold1,dold2,eavunew1
	real td19,td8,td88(npro,nsite),td20,td9,td99(npro,nsite)
	real td21,td10,td100(npro,nsite),td222,td101,td111(npro,nsite)
	real delnew,enew(npro,nsite),tn12,tn1,tn11(npro,nsite)
	real tnew4,tnew31,tnew32(npro,nsite),told102(npro,nsite)
	real tnew5,tnew51,tnew52(npro,nsite),tn13,tn2,tn22(npro,nsite)
	real tnew6,tnew61,tnew62(npro,nsite),tn14,tn3,tn33(npro,nsite)
	real tnew7,tnew71,tnew72(npro,nsite),tn15,tn4,tn44(npro,nsite)
	real tnew8,tnew81,tnew82(npro,nsite),tn16,tn5,tn55(npro,nsite)
	real tnew9,tnew91,tnew92(npro,nsite),tn17,tn6,tn66(npro,nsite)
	real tnew10,tnew101,tnew102(npro,nsite),enew2(npro)
	real tnew13,tnew131,tnew132(npro,nsite)
	real tnew12,tnew121,tnew122(npro,nsite),ss33,ss44
	real tnew,tnew1,tnew2(npro,nsite),tn18,tn7,tn77(npro,nsite)
	real tn19,tn8,tn88(npro,nsite),tn20,tn9,tn99(npro,nsite)
	real tn21,tn10,tn100(npro,nsite),tn222,tn101,tn111(npro,nsite)
	real tnew3,tnew11,tnew22(npro,nsite),delnew1
	integer bin_index(npro,nf,nsite),bin_index1(npro,nf,nsite)
	integer c(np),c1(np),c2(np1),c3(np2),c4(np3),c5(np4),c6(np5)
	integer c7(np6),c8(np7),c9(np8),c10(np9),c11(np10),c12(np12)
	integer c13(np13),c14(np14),c15(np15),c16(np16),c17(np17)
	integer c18(np18),c19(np19),c20(np20),c21(np21),c22(np22)
	integer s1(np),s2(np),ss1(np1),ss2(np1),s3s1(np2),s3s2(np2)
	integer s4s1(np3),s4s2(np3),s5s1(np4),s5s2(np4),s6s1(np5)
	integer s6s2(np5),s7s1(np6),s7s2(np6),s8s1(np7),s8s2(np7)
	integer s9s1(np8),s9s2(np8),s10s1(np9),s10s2(np9),s11s1(np10)
	integer s11s2(np10),s12s1(np12),s12s2(np12),s13s1(np13)
	integer s13s2(np13),s14s1(np14),s14s2(np14),s15s1(np15)
	integer s15s2(np15),s16s1(np16),s16s2(np16),s17s1(np17)
	integer s17s2(np17),s18s1(np18),s18s2(np18),s19s1(np19)
	integer s19s2(np19),s20s1(np20),s20s2(np20),s21s1(np21)
	integer s21s2(np21),s22s1(np22),s22s2(np22),arand1(nmut)
	real tpot2(na,na),tpot3(na,na),tpot4(na,na),tpot5(na,na)
	real tpot6(na,na),tpot7(na,na),tpot8(na,na),tpot9(na,na)
	real tpot10(na,na),tpot11(na,na),count1,count2,count3,count4
	real onepot(na,nf,nbin),tpot(na,na)
	character string1*12,string2*5,string3*6,string4*12
	
	kT1=0.05
	kT2=0.2
	kT3=0.001
	simcut=0.98
	simcut1=0.12

	open(1,file="bin-index-upto-nn10-2LHC")
	open(33,file="bin-index-upto-nn10-2LHD")
	open(2,file="one-body-potential-nn10-modified")
	open(7,file="contact-profile-2LHC-lt-5")
	open(34,file="contact-profile-2LHD-lt-5")
	open(3,file="new-potential-file-CA-0-5-modified")	
	open(8,file="contact-profile-2LHC-5-6")
	open(35,file="contact-profile-2LHD-5-6")
	open(9,file="new-potential-file-CA-5-6-modified")
	open(15,file="contact-profile-2LHC-6-7")
	open(36,file="contact-profile-2LHD-6-7")
	open(16,file="new-potential-file-CA-6-7-modified")
	open(17,file="contact-profile-2LHC-7-8")
	open(37,file="contact-profile-2LHD-7-8")
	open(18,file="new-potential-file-CA-7-8-modified")
	open(19,file="contact-profile-2LHC-8-9")
	open(38,file="contact-profile-2LHD-8-9")
	open(20,file="new-potential-file-CA-8-9-modified")
	open(21,file="contact-profile-2LHC-9-10")
	open(39,file="contact-profile-2LHD-9-10")
	open(22,file="new-potential-file-CA-9-10-modified")
	open(23,file="contact-profile-2LHC-10-11")
	open(40,file="contact-profile-2LHD-10-11")
	open(24,file="new-potential-file-CA-10-11-modified")
	open(25,file="contact-profile-2LHC-11-12")
	open(41,file="contact-profile-2LHD-11-12")
	open(26,file="new-potential-file-CA-11-12-modified")
	open(27,file="contact-profile-2LHC-12-13")
	open(42,file="contact-profile-2LHD-12-13")
	open(28,file="new-potential-file-CA-12-13-modified")
	open(29,file="contact-profile-2LHC-13-14")
	open(43,file="contact-profile-2LHD-13-14")
	open(30,file="new-potential-file-CA-13-14-modified")
	open(31,file="contact-profile-2LHC-14-15")
	open(44,file="contact-profile-2LHD-14-15")
	open(32,file="new-potential-file-CA-14-15-modified")
	open(45,file="input-2LHC-1.fasta")
	open(46,file="input-2LHD-1.fasta")
	
	do i=1,npro
	do j=1,nf
	do k=1,nsite
	read(1,*) bin_index(i,j,k)
c	write(*,*) bin_index(i,j,k)
	read(33,*)bin_index1(i,j,k)
c	write(*,*) bin_index1(i,j,k)
	enddo
	enddo
	enddo

c*********read-one-body-potentials-nearest neighbour****************
	do i=1,na
	do j=1,nf
	do k=1,nbin
	read(2,*) onepot(i,j,k)
	enddo
	enddo
	enddo
c***********************read-two-body-potential*************************
	do i=1,na
	do j=1,na
	read(3,"(31x,f11.6)") tpot(i,j)
	read(9,"(31x,f11.6)") tpot2(i,j)
	read(16,"(31x,f11.6)") tpot3(i,j)
	read(18,"(31x,f11.6)") tpot4(i,j)
	read(20,"(31x,f11.6)") tpot5(i,j)
	read(22,"(31x,f11.6)") tpot6(i,j)
	read(24,"(31x,f11.6)") tpot7(i,j)
	read(26,"(31x,f11.6)") tpot8(i,j)
	read(28,"(31x,f11.6)") tpot9(i,j)
	read(30,"(31x,f11.6)") tpot10(i,j)
	read(32,"(31x,f11.6)") tpot11(i,j)
	enddo
	enddo
c*******************read-contact-file-2LHC******************************
	do i=1,np
	read(7,*) c1(i),s1(i),s2(i)
c	write(*,*)c1(i),s1(i),s2(i)
	enddo
	do i=1,np1
	read(8,*) c2(i),ss1(i),ss2(i)
	enddo
	do i=1,np2
	read(15,*) c3(i),s3s1(i),s3s2(i)
	enddo
	do i=1,np3
	read(17,*) c4(i),s4s1(i),s4s2(i)
	enddo
	do i=1,np4
	read(19,*) c5(i),s5s1(i),s5s2(i)
	enddo
	do i=1,np5
	read(21,*) c6(i),s6s1(i),s6s2(i)
	enddo
	do i=1,np6
	read(23,*) c7(i),s7s1(i),s7s2(i)
	enddo
	do i=1,np7
	read(25,*) c8(i),s8s1(i),s8s2(i)
	enddo
	do i=1,np8
	read(27,*) c9(i),s9s1(i),s9s2(i)
	enddo
	do i=1,np9
	read(29,*) c10(i),s10s1(i),s10s2(i)
	enddo
	do i=1,np10
	read(31,*) c11(i),s11s1(i),s11s2(i)
	enddo
c*******************read-contact-file-2LHD******************************
	do i=1,np12
	read(34,*) c12(i),s12s1(i),s12s2(i)
c	write(*,*)c12(i),s12s1(i),s12s2(i)
	enddo
	do i=1,np13
	read(35,*) c13(i),s13s1(i),s13s2(i)
	enddo
	do i=1,np14
	read(36,*) c14(i),s14s1(i),s14s2(i)
	enddo
	do i=1,np15
	read(37,*) c15(i),s15s1(i),s15s2(i)
	enddo
	do i=1,np16
	read(38,*) c16(i),s16s1(i),s16s2(i)
	enddo
	do i=1,np17
	read(39,*) c17(i),s17s1(i),s17s2(i)
	enddo
	do i=1,np18
	read(40,*) c18(i),s18s1(i),s18s2(i)
	enddo
	do i=1,np19
	read(41,*) c19(i),s19s1(i),s19s2(i)
	enddo
	do i=1,np20
	read(42,*) c20(i),s20s1(i),s20s2(i)
	enddo
	do i=1,np21
	read(43,*) c21(i),s21s1(i),s21s2(i)
	enddo
	do i=1,np22
	read(44,*) c22(i),s22s1(i),s22s2(i)
c	write(*,*) c22(i),s22s1(i),s22s2(i)
	enddo
c*********************input-2JWS-2JWU**************************
	id=-2
	do k=1,mcseq
c	write(string2,"(I5)") k

	do i=1,nsite
	read(45,*) aseq(i)
	read(46,*) aseq1(i)
	enddo
c	do i=1,nsite
c	write(*,*)aseq(i),aseq1(i)
c	enddo
c****************************energy calculation********************************
c**two-body*Calpha-lt-5***take contact sites and their corresponding aa potential*********
c********************2LHC*********************
	told=0
	do j=1,np
	if(c1(j) .ne. c1(j-1).or.s1(j) .ne. s1(j-1)) then
	told=0
	endif
	
	told1=tpot(aseq(s1(j)),aseq(s2(j)))
	
	told2(c1(j),s1(j))= told + told1
	
	told= told2(c1(j),s1(j))	
	enddo
c********************2LHD***************************	
	td12=0
	do j=1,np12
	if(c12(j) .ne. c12(j-1).or.s12s1(j) .ne. s12s1(j-1)) then
	td12=0
	endif
	
	td1=tpot(aseq1(s12s1(j)),aseq1(s12s2(j)))
	
	td11(c12(j),s12s1(j))= td12 + td1
	
	td12= td11(c12(j),s12s1(j))
	enddo	
c******Calpha-5-6****two-body-potential*********************************
	told3=0
	do j=1,np1
	
	if(c2(j) .ne. c2(j-1).or.ss1(j).ne.ss1(j-1)) then
	told3=0
	endif
	
	told11=tpot2(aseq(ss1(j)),aseq(ss2(j)))
	
	told22(c2(j),ss1(j))= told3 + told11
	
	told3= told22(c2(j),ss1(j))	
	enddo
c********************2LHD***************************	
	td13=0
	do j=1,np13
	if(c13(j) .ne. c13(j-1).or.s13s1(j) .ne. s13s1(j-1)) then
	td13=0
	endif
	
	td2=tpot2(aseq1(s13s1(j)),aseq1(s13s2(j)))
	
	td22(c13(j),s13s1(j))= td13 + td2
	
	td13= td22(c13(j),s13s1(j))
	enddo
c******Calpha-6-7****two-body-potential*********************************
	told4=0
	do j=1,np2
	
	if(c3(j) .ne. c3(j-1).or. s3s1(j).ne. s3s1(j-1)) then
	told4=0
	endif
	
	told31=tpot3(aseq(s3s1(j)),aseq(s3s2(j)))
	
	told32(c3(j),s3s1(j))= told4 + told31
	
	told4= told32(c3(j),s3s1(j))
	enddo
c********************2LHD***************************	
	td14=0
	do j=1,np14
	if(c14(j) .ne. c14(j-1).or.s14s1(j) .ne. s14s1(j-1)) then
	td14=0
	endif
	
	td3=tpot3(aseq1(s14s1(j)),aseq1(s14s2(j)))
	
	td33(c14(j),s14s1(j))= td14 + td3
	
	td14= td33(c14(j),s14s1(j))
	enddo
c******Calpha-7-8****two-body-potential*********************************
	told5=0
	do j=1,np3
	
	if(c4(j) .ne. c4(j-1).or. s4s1(j).ne.s4s1(j-1)) then
	told5=0
	endif
	
	told51=tpot4(aseq(s4s1(j)),aseq(s4s2(j)))
	
	told52(c4(j),s4s1(j))= told5 + told51
	
	told5= told52(c4(j),s4s1(j))	
	enddo
c********************2LHD***************************	
	td15=0
	do j=1,np15
	if(c15(j) .ne. c15(j-1).or.s15s1(j) .ne. s15s1(j-1)) then
	td15=0
	endif
	
	td4=tpot4(aseq1(s15s1(j)),aseq1(s15s2(j)))
	
	td44(c15(j),s15s1(j))= td15 + td4
	
	td15= td44(c15(j),s15s1(j))
	enddo
c******Calpha-8-9****two-body-potential*********************************
	told6=0
	do j=1,np4
	
	if(c5(j) .ne. c5(j-1).or.s5s1(j).ne.s5s1(j-1)) then
	told6=0
	endif
	
	told61=tpot5(aseq(s5s1(j)),aseq(s5s2(j)))
	
	told62(c5(j),s5s1(j))= told6 + told61
	
	told6= told62(c5(j),s5s1(j))	
	enddo
c********************2LHD***************************	
	td16=0
	do j=1,np16
	if(c16(j) .ne. c16(j-1).or.s16s1(j) .ne. s16s1(j-1)) then
	td16=0
	endif
	
	td5=tpot5(aseq1(s16s1(j)),aseq1(s16s2(j)))
	
	td55(c16(j),s16s1(j))= td16 + td5
	
	td16= td55(c16(j),s16s1(j))
	enddo
c******Calpha-9-10****two-body-potential*********************************
	told7=0
	do j=1,np5
	
	if(c6(j) .ne. c6(j-1).or.s6s1(j).ne.s6s1(j-1)) then
	told7=0
	endif
	
	told71=tpot6(aseq(s6s1(j)),aseq(s6s2(j)))
	
	told72(c6(j),s6s1(j))= told7 + told71
	
	told7= told72(c6(j),s6s1(j))	
	enddo
c********************2LHD***************************	
	td17=0
	do j=1,np17
	if(c17(j) .ne. c17(j-1).or.s17s1(j) .ne. s17s1(j-1)) then
	td17=0
	endif
	td6=tpot6(aseq1(s17s1(j)),aseq1(s17s2(j)))
	td66(c17(j),s17s1(j))= td17 + td6
	td17= td66(c17(j),s17s1(j))
	enddo
c******Calpha-10-11****two-body-potential*********************************
	told8=0
	do j=1,np6
	
	if(c7(j) .ne. c7(j-1).or. s7s1(j) .ne.s7s1(j-1)) then
	told8=0
	endif
	
	told81=tpot7(aseq(s7s1(j)),aseq(s7s2(j)))
	
	told82(c7(j),s7s1(j))= told8 + told81
	
	told8= told82(c7(j),s7s1(j))	
	enddo
c********************2LHD***************************	
	td18=0
	do j=1,np18
	if(c18(j) .ne. c18(j-1).or.s18s1(j) .ne. s18s1(j-1)) then
	td18=0
	endif
	td7=tpot7(aseq1(s18s1(j)),aseq1(s18s2(j)))
	td77(c18(j),s18s1(j))= td18 + td7
	td18= td77(c18(j),s18s1(j))
	enddo
c******Calpha-11-12****two-body-potential*********************************
	told9=0
	do j=1,np7
	
	if(c8(j) .ne. c8(j-1).or.s8s1(j).ne.s8s1(j-1)) then
	told9=0
	endif
	
	told91=tpot8(aseq(s8s1(j)),aseq(s8s2(j)))
	
	told92(c8(j),s8s1(j))= told9 + told91
	
	told9= told92(c8(j),s8s1(j))	
	enddo
c********************2LHD***************************	
	td19=0
	do j=1,np19
	if(c19(j) .ne. c19(j-1).or.s19s1(j) .ne. s19s1(j-1)) then
	td19=0
	endif
	td8=tpot8(aseq1(s19s1(j)),aseq1(s19s2(j)))
	td88(c19(j),s19s1(j))= td19 + td8
	td19= td88(c19(j),s19s1(j))
	enddo
c******Calpha-12-13****two-body-potential*********************************
	told10=0
	do j=1,np8
	
	if(c9(j) .ne. c9(j-1).or.s9s1(j).ne. s9s1(j-1)) then
	told10=0
	endif
	
	told101=tpot9(aseq(s9s1(j)),aseq(s9s2(j)))
	
	told102(c9(j),s9s1(j))= told10 + told101
	
	told10= told102(c9(j),s9s1(j))	
	enddo
c********************2LHD***************************	
	td20=0
	do j=1,np20
	if(c20(j) .ne. c20(j-1).or.s20s1(j) .ne. s20s1(j-1)) then
	td20=0
	endif
	td9=tpot9(aseq1(s20s1(j)),aseq1(s20s2(j)))
	td99(c20(j),s20s1(j))= td20 + td9
	td20= td99(c20(j),s20s1(j))
	enddo
c******Calpha-13-14****two-body-potential*********************************
	told13=0
	do j=1,np9
	
	if(c10(j) .ne. c10(j-1).or. s10s1(j).ne.s10s1(j-1)) then
	told13=0
	endif
	
	told131=tpot10(aseq(s10s1(j)),aseq(s10s2(j)))
	
	told132(c10(j),s10s1(j))= told13 + told131
	
	told13= told132(c10(j),s10s1(j))	
	enddo
c********************2LHD***************************	
	td21=0
	do j=1,np21
	if(c21(j) .ne. c21(j-1).or.s21s1(j) .ne. s21s1(j-1)) then
	td21=0
	endif
	td10=tpot10(aseq1(s21s1(j)),aseq1(s21s2(j)))
	td100(c21(j),s21s1(j))= td21 + td10
	td21= td100(c21(j),s21s1(j))
	enddo
c******Calpha-14-15****two-body-potential*********************************
	told12=0
	do j=1,np10
	
	if(c11(j) .ne. c11(j-1).or. s11s1(j).ne. s11s1(j-1)) then
	told12=0
	endif
	
	told121=tpot11(aseq(s11s1(j)),aseq(s11s2(j)))
	
	told122(c11(j),s11s1(j))= told12 + told121
	
	told12= told122(c11(j),s11s1(j))	
	enddo
c********************2LHD***************************	
	td222=0
	do j=1,np22
	if(c22(j) .ne. c22(j-1).or.s22s1(j) .ne. s22s1(j-1)) then
	td222=0
	endif
	td101=tpot11(aseq1(s22s1(j)),aseq1(s22s2(j)))
	td111(c22(j),s22s1(j))= td222 + td101
	td222= td111(c22(j),s22s1(j))
	enddo
c******onebody-nearest-number-dependent-upto-nn10*****************************
c********************2LHC***********************************
	do i=1,npro
	do j=1,nsite
	if(i .ne. i-1) then
	oneold=0
	endif
	do k1=1,nf
	
	oneold1=onepot(aseq(j),k1,bin_index(i,k1,j))     	
     	oneold2(i,j)=oneold + oneold1
     	
     	oneold=oneold2(i,j)
	enddo
	enddo
	enddo
c**********************2LHD***********************************
	do i=1,npro
	do j=1,nsite
	if(i .ne. i-1) then
	oned=0
	endif
	do k1=1,nf
	
	oned1=onepot(aseq1(j),k1,bin_index1(i,k1,j))     	
     	oned2(i,j)=oned + oned1
     	
     	oned=oned2(i,j)
	enddo
	enddo
	enddo
c	do j=1,nsite
c	write(*,*) oneold2(1,j),oned2(1,j)
c	enddo
c**********************************add-old-energy******************************
c****************************2LHC***************************************	
	en1=0
	count1=0
	do i=1,npro
	if (i .ne. i-1) then
	en1=0
	endif
	
	do j=1,nsite	
	ss11= told2(i,j)+told22(i,j)+told32(i,j)+told52(i,j)
     $  +told62(i,j)+told72(i,j)+told82(i,j)+told92(i,j)
     $  +told102(i,j)+told132(i,j)+told122(i,j)+oneold2(i,j)
     	
     	eold1(i)=en1+ss11
     	en1=eold1(i)
	enddo
	if (eold1(i) .lt. eold1(1)) then
	count1=count1+1
	endif
	enddo
c	open(50,file="energy")
c	do i=1,npro
c	write(50,*) eold1(i)
c	enddo
	
	eun1=0
	do i=2,npro
	eun1=eun1 + eold1(i)
	enddo
	
	eavun1=eun1/nupro
	delold1= eold1(1)-eavun1	
c****************************2LHD***************************************	
	en2=0
	count2=0
	do i=1,npro
	if (i .ne. i-1) then
	en2=0
	endif
	
	do j=1,nsite	
	ss22= td11(i,j)+td22(i,j)+td33(i,j)+td44(i,j)
     $  +td55(i,j)+td66(i,j)+td77(i,j)+td88(i,j)
     $  +td99(i,j)+td100(i,j)+td111(i,j)+oned2(i,j)
	
	eold2(i)=en2+ss22
	en2=eold2(i)
	enddo
	if (eold2(i) .lt. eold2(1)) then
	count2=count2+1
	endif
	enddo
	
	eun2=0
	do i=2,npro
	eun2=eun2 + eold2(i)
	enddo
	
	eavun2=eun2/nupro
	
	delold2= eold2(1)-eavun2
c	write(*,*) eold1(1),eold2(1),eun1,eun2,delold1,delold2
c**************check sequence similarity*********************
	sim1=0
	do i=1,nsite
	if(aseq(i) .eq. aseq1(i))then
	sim1=sim1+1
	endif	
	enddo
	
	simold=abs((sim1/nsite)-simcut)
c	open(51,file="initial-random-sequence")
c	do i=1,nsite
c	write(51,*) aseq(i),aseq1(i)
c	enddo
c	write(*,*) sim1,simold
	write(*,*)delold1,delold2,simold
c******************************random site selection-2LHC************************
	do l=1,mcstep
	
	do i=1,nmut
c	write(*,*) i	
	cc1=ran1(id)
	site1=int(cc1*nsite)+1
	ssite1(i)=site1
c	write(*,*) ssite1(i)
	enddo
c*********save old seq as a new sequence-2LHC*******************************
	do i=1,nsite
	aseqn(i)=aseq(i)
c	write(*,*) aseqn(i),aseq(i)
	enddo
c**********************random amino acid selection at selected site**************
	do i=1,nmut
	cc2=ran1(id)
	arand(i)=int(cc2*20)+1
c	write(*,*) arand(i)
	aseqn(ssite1(i))=arand(i)
	enddo
c	do i=1,nsite
c	write (*,*) aseqn(i),aseq(i)
c	enddo
c******************************random site selection-2LHD************************
	do i=1,nmut	
	cc4=ran1(id)
	site2=int(cc4*nsite)+1
	ssite2(i)=site2
c	write(*,*) ssite2(i)
	enddo
c*********save old seq as a new sequence-2LHD*******************************
	do i=1,nsite
	aseqn1(i)=aseq1(i)
c	write(*,*) aseqn1(i),aseq1(i)
	enddo
c*************random amino acid selection at selected site*****************
	do i=1,nmut
	cc5=ran1(id)
	arand1(i)=int(cc5*20)+1
c	write(*,*) arand1(i)
	aseqn1(ssite2(i))=arand1(i)
	enddo
c	do i=1,nsite
c	write (*,*) aseqn1(i),aseq1(i)
c	enddo
c*********************save old sequence energy*****************************
c	dold1=delold1
c	dold2=delold2
c	write(*,*) dold1,delold1,dold2,delold2

c*************************calculate energy of new sequence*****************
c********two-body-pot*****Calpha-lt-5-2LHC**************
	tnew=0
	do j=1,np
	if(c1(j).ne.c1(j-1).or.s1(j).ne.s1(j-1)) then
	tnew=0
	endif
	tnew1=tpot(aseqn(s1(j)),aseqn(s2(j)))
	tnew2(c1(j),s1(j))= tnew + tnew1
	tnew= tnew2(c1(j),s1(j))	
	enddo
c********************2LHD***************************	
	tn12=0
	do j=1,np12
	if(c12(j) .ne. c12(j-1).or.s12s1(j) .ne. s12s1(j-1)) then
	tn12=0
	endif
	tn1=tpot(aseqn1(s12s1(j)),aseqn1(s12s2(j)))
	tn11(c12(j),s12s1(j))= tn12 + tn1
	tn12= tn11(c12(j),s12s1(j))
	enddo	
c****************Calpha-5-6***2LHC*****two-body-pot*********
	tnew3=0
	do j=1,np1	
	if(c2(j) .ne. c2(j-1).or.ss1(j).ne.ss1(j-1)) then
	tnew3=0
	endif
	tnew11=tpot2(aseqn(ss1(j)),aseqn(ss2(j)))
	tnew22(c2(j),ss1(j))= tnew3 + tnew11
	tnew3= tnew22(c2(j),ss1(j))	
	enddo
c********************2LHD***************************	
	tn13=0
	do j=1,np13
	if(c13(j) .ne. c13(j-1).or.s13s1(j) .ne. s13s1(j-1)) then
	tn13=0
	endif
	tn2=tpot2(aseqn1(s13s1(j)),aseqn1(s13s2(j)))
	tn22(c13(j),s13s1(j))= tn13 + tn2
	tn13= tn22(c13(j),s13s1(j))
	enddo
c****************Calpha-6-7******two-body-pot*********
	tnew4=0
	do j=1,np2
	if(c3(j) .ne. c3(j-1).or. s3s1(j) .ne. s3s1(j-1)) then
	tnew4=0
	endif
	tnew31=tpot3(aseqn(s3s1(j)),aseqn(s3s2(j)))
	tnew32(c3(j),s3s1(j))= tnew4 + tnew31
	tnew4= tnew32(c3(j),s3s1(j))	
	enddo
c********************2LHD***************************	
	tn14=0
	do j=1,np14
	if(c14(j) .ne. c14(j-1).or.s14s1(j) .ne. s14s1(j-1)) then
	tn14=0
	endif
	tn3=tpot3(aseqn1(s14s1(j)),aseqn1(s14s2(j)))
	tn33(c14(j),s14s1(j))= tn14 + tn3
	tn14= tn33(c14(j),s14s1(j))
	enddo
c****************Calpha-7-8******two-body-pot*********
	tnew5=0
	do j=1,np3
	if(c4(j).ne.c4(j-1).or.s4s1(j).ne.s4s1(j-1) ) then
	tnew5=0
	endif
	tnew51=tpot4(aseqn(s4s1(j)),aseqn(s4s2(j)))
	tnew52(c4(j),s4s1(j))= tnew5 + tnew51
	tnew5= tnew52(c4(j),s4s1(j))	
	enddo
c********************2LHD***************************	
	tn15=0
	do j=1,np15
	if(c15(j) .ne. c15(j-1).or.s15s1(j) .ne. s15s1(j-1)) then
	tn15=0
	endif
	tn4=tpot4(aseqn1(s15s1(j)),aseqn1(s15s2(j)))
	tn44(c15(j),s15s1(j))= tn15 + tn4
	tn15= tn44(c15(j),s15s1(j))
	enddo
c****************Calpha-8-9******two-body-pot*********
	tnew6=0
	do j=1,np4
	
	if(c5(j) .ne. c5(j-1).or.s5s1(j).ne.s5s1(j-1)) then
	tnew6=0
	endif
	tnew61=tpot5(aseqn(s5s1(j)),aseqn(s5s2(j)))
	tnew62(c5(j),s5s1(j))= tnew6 + tnew61
	tnew6= tnew62(c5(j),s5s1(j))	
	enddo
c********************2LHD***************************	
	tn16=0
	do j=1,np16
	if(c16(j) .ne. c16(j-1).or.s16s1(j) .ne. s16s1(j-1)) then
	tn16=0
	endif
	tn5=tpot5(aseqn1(s16s1(j)),aseqn1(s16s2(j)))
	tn55(c16(j),s16s1(j))= tn16 + tn5
	tn16= tn55(c16(j),s16s1(j))
	enddo
c****************Calpha-9-10******two-body-pot*********
	tnew7=0
	do j=1,np5
	
	if(c6(j) .ne. c6(j-1).or. s6s1(j).ne.s6s1(j-1)) then
	tnew7=0
	endif
	tnew71=tpot6(aseqn(s6s1(j)),aseqn(s6s2(j)))
	tnew72(c6(j),s6s1(j))= tnew7 + tnew71
	tnew7= tnew72(c6(j),s6s1(j))	
	enddo
c********************2LHD***************************	
	tn17=0
	do j=1,np17
	if(c17(j) .ne. c17(j-1).or.s17s1(j) .ne. s17s1(j-1)) then
	tn17=0
	endif
	tn6=tpot6(aseqn1(s17s1(j)),aseqn1(s17s2(j)))
	tn66(c17(j),s17s1(j))= tn17 + tn6
	tn17= tn66(c17(j),s17s1(j))
	enddo
c****************Calpha-10-11******two-body-pot*********
	tnew8=0
	do j=1,np6
	
	if(c7(j) .ne. c7(j-1).or.s7s1(j).ne.s7s1(j-1)) then
	tnew8=0
	endif
	tnew81=tpot7(aseqn(s7s1(j)),aseqn(s7s2(j)))
	tnew82(c7(j),s7s1(j))= tnew8 + tnew81
	tnew8= tnew82(c7(j),s7s1(j))	
	enddo
c********************2LHD***************************	
	tn18=0
	do j=1,np18
	if(c18(j) .ne. c18(j-1).or.s18s1(j) .ne. s18s1(j-1)) then
	tn18=0
	endif
	tn7=tpot7(aseqn1(s18s1(j)),aseqn1(s18s2(j)))
	tn77(c18(j),s18s1(j))= tn18 + tn7
	tn18= tn77(c18(j),s18s1(j))
	enddo
c****************Calpha-11-12******two-body-pot*********
	tnew9=0
	do j=1,np7
	
	if(c8(j) .ne. c8(j-1).or.s8s1(j).ne.s8s1(j-1)) then
	tnew9=0
	endif
	
	tnew91=tpot8(aseqn(s8s1(j)),aseqn(s8s2(j)))
	
	tnew92(c8(j),s8s1(j))= tnew9 + tnew91
	
	tnew9= tnew92(c8(j),s8s1(j))	
	enddo
c********************2LHD***************************	
	tn19=0
	do j=1,np19
	if(c19(j) .ne. c19(j-1).or.s19s1(j) .ne. s19s1(j-1)) then
	tn19=0
	endif
	tn8=tpot8(aseqn1(s19s1(j)),aseqn1(s19s2(j)))
	tn88(c19(j),s19s1(j))= tn19 + tn8
	tn19= tn88(c19(j),s19s1(j))
	enddo
c****************Calpha-12-13******two-body-pot*********
	tnew10=0
	do j=1,np8

	if(c9(j) .ne. c9(j-1).or.s9s1(j).ne.s9s1(j-1)) then
	tnew10=0
	endif
	
	tnew101=tpot9(aseqn(s9s1(j)),aseqn(s9s2(j)))
	
	tnew102(c9(j),s9s1(j))= tnew10 + tnew101
	
	tnew10= tnew102(c9(j),s9s1(j))
	enddo
c********************2LHD***************************	
	tn20=0
	do j=1,np20
	if(c20(j) .ne. c20(j-1).or.s20s1(j) .ne. s20s1(j-1)) then
	tn20=0
	endif
	tn9=tpot9(aseqn1(s20s1(j)),aseqn1(s20s2(j)))
	tn99(c20(j),s20s1(j))= tn20 + tn9
	tn20= tn99(c20(j),s20s1(j))
	enddo
c****************Calpha-13-14******two-body-pot*********
	tnew13=0
	do j=1,np9

	if(c10(j) .ne. c10(j-1).or.s10s1(j).ne.s10s1(j-1)) then
	tnew13=0
	endif
	tnew131=tpot10(aseqn(s10s1(j)),aseqn(s10s2(j)))
	tnew132(c10(j),s10s1(j))= tnew13 + tnew131
	tnew13= tnew132(c10(j),s10s1(j))	
	enddo
c********************2LHD***************************	
	tn21=0
	do j=1,np21
	if(c21(j) .ne. c21(j-1).or.s21s1(j) .ne. s21s1(j-1)) then
	tn21=0
	endif
	tn10=tpot10(aseqn1(s21s1(j)),aseqn1(s21s2(j)))
	tn100(c21(j),s21s1(j))= tn21 + tn10
	tn21= tn100(c21(j),s21s1(j))
	enddo
c****************Calpha-14-15******two-body-pot*********
	tnew12=0
	do j=1,np10

	if(c11(j) .ne. c11(j-1).or.s11s1(j).ne.s11s1(j-1)) then
	tnew12=0
	endif
	tnew121=tpot11(aseqn(s11s1(j)),aseqn(s11s2(j)))
	tnew122(c11(j),s11s1(j))= tnew12 + tnew121
	tnew12= tnew122(c11(j),s11s1(j))	
	enddo
c********************2LHD***************************	
	tn222=0
	do j=1,np22
	if(c22(j) .ne. c22(j-1).or.s22s1(j) .ne. s22s1(j-1)) then
	tn222=0
	endif
	tn101=tpot11(aseqn1(s22s1(j)),aseqn1(s22s2(j)))
	tn111(c22(j),s22s1(j))= tn222 + tn101
	tn222= tn111(c22(j),s22s1(j))
	enddo
c******onebody-nearest-number-dependent-upto-nn10*****************************
c********************2LHC***********************************
	do i=1,npro
	do j=1,nsite
	if(i .ne. i-1) then
	onenew=0
	endif
	do k1=1,nf
	
	onenew1=onepot(aseqn(j),k1,bin_index(i,k1,j))     	
     	onenew2(i,j)=onenew + onenew1
     	onenew=onenew2(i,j)
	enddo
	enddo
	enddo
c**********************2LHD***********************************
	do i=1,npro
	do j=1,nsite
	if(i .ne. i-1) then
	onen=0
	endif
	do k1=1,nf
	
	onen1=onepot(aseqn1(j),k1,bin_index1(i,k1,j))     	
     	onen2(i,j)=onen + onen1	
     	onen=onen2(i,j)
	enddo
	enddo
	enddo
c	do j=1,nsite
c	write(*,*) onenew2(1,j),onen2(1,j)
c	enddo
	
c**********************************add-new-energy******************************
c****************************2LHC***************************************	
	enw1=0
	count3=0
	do i=1,npro
	if (i .ne. i-1) then
	enw1=0
	endif
	
	do j=1,nsite	
	ss33= tnew2(i,j)+tnew22(i,j)+tnew32(i,j)+tnew52(i,j)
     $   +tnew62(i,j)+tnew72(i,j)+tnew82(i,j)+tnew92(i,j)
     $   +tnew102(i,j)+tnew132(i,j)+tnew122(i,j)+onenew2(i,j)
     
     	enew1(i)=enw1+ss33
     	enw1=enew1(i)
	enddo
	if (enew1(i) .lt. enew1(1)) then
	count3=count3+1
	endif
	enddo
c	open(50,file="energy")
c	do i=1,npro
c	write(50,*) enew1(i)
c	enddo
	
	eunew1=0
	do i=2,npro
	eunew1=eunew1 + enew1(i)
	enddo
	
	eavunew1=eunew1/nupro
	delnew1= enew1(1)-eavunew1	
c****************************2LHD***************************************	
	enw2=0
	count4=0
	do i=1,npro
	if (i .ne. i-1) then
	enw2=0
	endif
	
	do j=1,nsite	
	ss44= tn11(i,j)+tn22(i,j)+tn33(i,j)+tn44(i,j)
     $  +tn55(i,j)+tn66(i,j)+tn77(i,j)+tn88(i,j)
     $  +tn99(i,j)+tn100(i,j)+tn111(i,j)+onen2(i,j)
	
	enew2(i)=enw2+ss44
	enw2=enew2(i)
	enddo
	if (enew2(i) .lt. enew2(1)) then

	count4=count4+1
	endif
	enddo
	
	eunew2=0
	do i=2,npro
	eunew2=eunew2 + enew2(i)
	enddo
	
	eavunew2=eunew2/nupro
	delnew2= enew2(1)-eavunew2
c	write(*,*) enew1(1),enew2(1),eunew1,eunew2,delnew1,delnew2

c**************check sequence similarity*********************
	simn1=0
	do i=1,nsite
	if(aseqn(i) .eq. aseqn1(i))then
	simn1=simn1+1
	endif	
	enddo
	
	simnew=abs((simn1/nsite)-simcut)
	write(*,*)delnew1,delnew2,simnew
c********************condition of acceptance and rejection****************
c*********************lll******************************
	if (delnew1.le.delold1.and.delnew2.le.delold2
     $   .and.simnew.le.simold) then

	do i=1,nsite
	aseq(i)=aseqn(i)
	aseq1(i)=aseqn1(i)
	enddo
	
	delold1=delnew1
	delold2=delnew2
	simold=simnew

	write(string1,"(F12.4)") delnew1
	write(string4,"(F12.4)") delnew2
	write(string3,"(F6.4)") simnew
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	open(60,file="sim-check")
	write(60,*) string3
	
	if (simnew .le. simcut1) then
	write(*,*) "accept"
	open(12,file="mc-seq/"//"2LHC-"//string1//"-"//string3)
	open(13,file="mc-seq/"//"2LHD-"//string4//"-"//string3)
	open(14,file="nc1-nc2-sim/"//"nc-"//string1//"-"//string4)
	write(14,*) count3,count4,simnew	
	
	do i=1,nsite
	write(12,*) aseqn(i)
	write(13,*) aseqn1(i)
	enddo

	endif
c****************lgg**************************	
	elseif(delnew1.le.delold1.and.delnew2.ge.delold2
     $   .and.simnew.ge.simold) then
	
	random=ran1(id)
	p2=exp(-((delnew2-delold2))/kT2)
	p3=exp(-(simnew-simold)/kT3)
	p=p2*p3
	if(random .le. p)then
c	write(*,*) p,random
	do i=1,nsite
	aseq(i)=aseqn(i)
	aseq1(i)=aseqn1(i)
	enddo

	delold1=delnew1
	delold2=delnew2
	simold=simnew

	write(string1,"(F12.4)") delnew1
	write(string4,"(F12.4)") delnew2
	write(string3,"(F6.4)") simnew
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	open(60,file="sim-check")
	write(60,*) string3
	
	if (simnew .le. simcut1) then
	write(*,*) "accept"
	open(12,file="mc-seq/"//"2LHC-"//string1//"-"//string3)
	open(13,file="mc-seq/"//"2LHD-"//string4//"-"//string3)
	open(14,file="nc1-nc2-sim/"//"nc-"//string1//"-"//string4)
	write(14,*) count3,count4,simnew
	
	do i=1,nsite
	write(12,*) aseqn(i)
	write(13,*) aseqn1(i)
	enddo
	endif
	
	else

	do i=1,nsite
	aseqn(i)=aseq(i)
	aseqn1(i)=aseq1(i)
	enddo

	delnew1=delold1
	delnew2=delold2
	simnew=simold
	
	write(string1,"(F12.4)") delold1
	write(string4,"(F12.4)") delold2
	write(string3,"(F6.4)") simold
	open(60,file="sim-check")
	write(60,*) string3
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	write(*,*) "reject"
	
	endif

c*********************ggg************************
	elseif(delnew1.ge.delold1.and.delnew2.ge.delold2
     $   .and.simnew.ge.simold) then
	
	random=ran1(id)
	p1=exp(-((delnew1-delold1))/kT1)
	p2=exp(-((delnew2-delold2))/kT2)
	p3=exp(-(simnew-simold)/kT3)
c	write(*,*) kT3
	p=p1 * p2 * p3
c	write(*,*) p1,p2,p
c	pfinal=min(1.0, p)
	if(random .le. p)then
	write(*,*) p,random,kT3
	do i=1,nsite
	aseq(i)=aseqn(i)
	aseq1(i)=aseqn1(i)
	enddo

	delold1=delnew1
	delold2=delnew2
	simold=simnew

	write(string1,"(F12.4)") delnew1
	write(string4,"(F12.4)") delnew2
	write(string3,"(F6.4)") simnew
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	open(60,file="sim-check")
	write(60,*) string3
	
	if (simnew.le. simcut1) then
	write(*,*) "accept"
	open(12,file="mc-seq/"//"2LHC-"//string1//"-"//string3)
	open(13,file="mc-seq/"//"2LHD-"//string4//"-"//string3)
	open(14,file="nc1-nc2-sim/"//"nc-"//string1//"-"//string4)
	write(14,*) count3,count4,simnew

	do i=1,nsite
	write(12,*) aseqn(i)
	write(13,*) aseqn1(i)
	enddo
	endif
	
	else

	do i=1,nsite
	aseqn(i)=aseq(i)
	aseqn1(i)=aseq1(i)
	enddo

	delnew1=delold1
	delnew2=delold2
	simnew=simold
	
	write(string1,"(F12.4)") delold1
	write(string4,"(F12.4)") delold2
	write(string3,"(F6.4)") simold
	open(60,file="sim-check")
	write(60,*) string3
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	write(*,*) "reject"

	endif

c********************ggl***********************
	elseif(delnew1.ge.delold1.and.delnew2.ge.delold2
     $   .and.simnew.le.simold) then
	
	random=ran1(id)
	p1=exp(-((delnew1-delold1))/kT1)
	p2=exp(-((delnew2-delold2))/kT2)

	p=p1 * p2
c	pfinal=min(1.0, p)
	if(random .le. p)then
c	write(*,*) p,random
	do i=1,nsite
	aseq(i)=aseqn(i)
	aseq1(i)=aseqn1(i)
	enddo

	delold1=delnew1
	delold2=delnew2
	simold=simnew

	write(string1,"(F12.4)") delnew1
	write(string4,"(F12.4)") delnew2
	write(string3,"(F6.4)") simnew
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	open(60,file="sim-check")
	write(60,*) string3
	
	if (simnew .le. simcut1) then
	write(*,*) "accept"
	open(12,file="mc-seq/"//"2LHC-"//string1//"-"//string3)
	open(13,file="mc-seq/"//"2LHD-"//string4//"-"//string3)
	open(14,file="nc1-nc2-sim/"//"nc-"//string1//"-"//string4)
	write(14,*) count3,count4,simnew
	
	do i=1,nsite
	write(12,*) aseqn(i)
	write(13,*) aseqn1(i)
	enddo
	endif
	
	else

	do i=1,nsite
	aseqn(i)=aseq(i)
	aseqn1(i)=aseq1(i)
	enddo

	delnew1=delold1
	delnew2=delold2
	simnew=simold
	
	write(string1,"(F12.4)") delold1
	write(string4,"(F12.4)") delold2
	write(string3,"(F6.4)") simold
	open(60,file="sim-check")
	write(60,*) string3
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	write(*,*) "reject"
	
	endif

c********************gll***********************
	elseif(delnew1.ge.delold1.and.delnew2.le.delold2
     $   .and.simnew.le.simold) then
	
	random=ran1(id)
	p1=exp(-((delnew1-delold1))/kT1)
	p=p1
c	write(*,*) p1,p3,p
	if(random .le. p)then
c	write(*,*) p,random
	do i=1,nsite
	aseq(i)=aseqn(i)
	aseq1(i)=aseqn1(i)
	enddo

	delold1=delnew1
	delold2=delnew2
	simold=simnew

	write(string1,"(F12.4)") delnew1
	write(string4,"(F12.4)") delnew2
	write(string3,"(F6.4)") simnew
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	open(60,file="sim-check")
	write(60,*) string3
	
	if (simnew .le. simcut1) then
	write(*,*) "accept"
	open(12,file="mc-seq/"//"2LHC-"//string1//"-"//string3)
	open(13,file="mc-seq/"//"2LHD-"//string4//"-"//string3)
	open(14,file="nc1-nc2-sim/"//"nc-"//string1//"-"//string4)
	write(14,*) count3,count4,simnew
	
	do i=1,nsite
	write(12,*) aseqn(i)
	write(13,*) aseqn1(i)
	enddo
	endif

	else

	do i=1,nsite
	aseqn(i)=aseq(i)
	aseqn1(i)=aseq1(i)
	enddo

	delnew1=delold1
	delnew2=delold2
	simnew=simold
	
	write(string1,"(F12.4)") delold1
	write(string4,"(F12.4)") delold2
	write(string3,"(F6.4)") simold
	open(60,file="sim-check")
	write(60,*) string3
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	write(*,*) "reject"
	
	endif
c********************llg***********************
	elseif(delnew1.le.delold1.and.delnew2.le.delold2
     $   .and.simnew.ge.simold) then
	
	random=ran1(id)
	p3=exp(-(simnew-simold)/kT3)
	p=p3
c	write(*,*) p3,p
c	pfinal=min(1.0, p)
	if(random .le. p)then
c	write(*,*) p,random
	do i=1,nsite
	aseq(i)=aseqn(i)
	aseq1(i)=aseqn1(i)
	enddo

	delold1=delnew1
	delold2=delnew2
	simold=simnew

	write(string1,"(F12.4)") delnew1
	write(string4,"(F12.4)") delnew2
	write(string3,"(F6.4)") simnew
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	open(60,file="sim-check")
	write(60,*) string3
	
	if (simnew .le. simcut1) then
	write(*,*) "accept"
	open(12,file="mc-seq/"//"2LHC-"//string1//"-"//string3)
	open(13,file="mc-seq/"//"2LHD-"//string4//"-"//string3)
	open(14,file="nc1-nc2-sim/"//"nc-"//string1//"-"//string4)
	write(14,*) count3,count4,simnew
	
	do i=1,nsite
	write(12,*) aseqn(i)
	write(13,*) aseqn1(i)
	enddo
	endif
	
	else

	do i=1,nsite
	aseqn(i)=aseq(i)
	aseqn1(i)=aseq1(i)
	enddo

	delnew1=delold1
	delnew2=delold2
	simnew=simold
	
	write(string1,"(F12.4)") delold1
	write(string4,"(F12.4)") delold2
	write(string3,"(F6.4)") simold
	open(60,file="sim-check")
	write(60,*) string3
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	write(*,*) "reject"
	
	endif
c********************lgl***********************
	elseif(delnew1.le.delold1.and.delnew2.ge.delold2
     $   .and.simnew.le.simold) then
	
	random=ran1(id)
	p2=exp(-((delnew2-delold2))/kT2)
	p= p2
c	write(*,*) p2,p3,p
c	pfinal=min(1.0, p)
	if(random .le. p)then
c	write(*,*) p,random
	do i=1,nsite
	aseq(i)=aseqn(i)
	aseq1(i)=aseqn1(i)
	enddo

	delold1=delnew1
	delold2=delnew2
	simold=simnew

	write(string1,"(F12.4)") delnew1
	write(string4,"(F12.4)") delnew2
	write(string3,"(F6.4)") simnew
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	open(60,file="sim-check")
	write(60,*) string3	
	
	if (simnew .le. simcut1) then
	write(*,*) "accept"
	open(12,file="mc-seq/"//"2LHC-"//string1//"-"//string3)
	open(13,file="mc-seq/"//"2LHD-"//string4//"-"//string3)
	open(14,file="nc1-nc2-sim/"//"nc-"//string1//"-"//string4)
	write(14,*) count3,count4,simnew
	
	do i=1,nsite
	write(12,*) aseqn(i)
	write(13,*) aseqn1(i)
	enddo
	endif
	
	else

	do i=1,nsite
	aseqn(i)=aseq(i)
	aseqn1(i)=aseq1(i)
	enddo

	delnew1=delold1
	delnew2=delold2
	simnew=simold
	
	write(string1,"(F12.4)") delold1
	write(string4,"(F12.4)") delold2
	write(string3,"(F6.4)") simold
	open(60,file="sim-check")
	write(60,*) string3
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	write(*,*) "reject"
	
	endif
c********************glg***********************
	elseif(delnew1.ge.delold1.and.delnew2.le.delold2
     $   .and.simnew.ge.simold) then
	
	random=ran1(id)
	p1=exp(-((delnew1-delold1))/kT1)
	p3=exp(-(simnew-simold)/kT3)
	p=p1*p3
c	write(*,*) p1
c	pfinal=min(1.0, p)
	if(random .le. p)then
c	write(*,*) p,random
	do i=1,nsite
	aseq(i)=aseqn(i)
	aseq1(i)=aseqn1(i)
	enddo

	delold1=delnew1
	delold2=delnew2
	simold=simnew

	write(string1,"(F12.4)") delnew1
	write(string4,"(F12.4)") delnew2
	write(string3,"(F6.4)") simnew
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	open(60,file="sim-check")
	write(60,*) string3
	
	if (simnew .le. simcut1) then
	write(*,*) "accept"
	open(12,file="mc-seq/"//"2LHC-"//string1//"-"//string3)
	open(13,file="mc-seq/"//"2LHD-"//string4//"-"//string3)
	open(14,file="nc1-nc2-sim/"//"nc-"//string1//"-"//string4)
	write(14,*) count3,count4,simnew
	
	do i=1,nsite
	write(12,*) aseqn(i)
	write(13,*) aseqn1(i)
	enddo
	endif
	
	else

	do i=1,nsite
	aseqn(i)=aseq(i)
	aseqn1(i)=aseq1(i)
	enddo

	delnew1=delold1
	delnew2=delold2
	simnew=simold
	
	write(string1,"(F12.4)") delold1
	write(string4,"(F12.4)") delold2
	write(string3,"(F6.4)") simold
	open(60,file="sim-check")
	write(60,*) string3
	
	open(10,file="del-mc-step/"//"del-mc-step-2LHC")
	open(11,file="del-mc-step/"//"del-mc-step-2LHD")
	write(10,*) string1
	write(11,*) string4
	write(*,*) "reject"
	
	endif
	endif
	enddo

	enddo
	
	stop
	end
	
	
	function ran1(idum)

	integer idum,ia,im,iq,ir,ntab,ndiv
	real    ran1,am,eps,rnmx
c
      	parameter (ia=16807,im=2147483647,am=1./im,iq=127773,ir=2836)
      	parameter (ntab=32,ndiv=1+(im-1)/ntab,eps=1.2e-7,rnmx=1.-eps)
c
c     Numerical Rec. in Fortran, 2nd eD.  P. 271
c
      	integer j,k
      	integer iv(ntab),iy
      	save    iv,iy
      	data    iv,iy /ntab*0,0/
c
      	if (idum.le.0.or.iy.eq.0) then
         idum=max(-idum,1)
         do j=ntab+8,1,-1
            k    = idum/iq
            idum = ia*(idum-k*iq)-ir*k
            if(idum.lt.0) idum = idum+im
            if (j.le.ntab) iv(j) = idum
         enddo
         iy = iv(1)
      	endif
      	k    = idum/iq
      	idum = ia*(idum-k*iq)-ir*k
      	if(idum.lt.0) idum = idum+im
      	j     = 1+iy/ndiv
      	iy    = iv(j)
      	iv(j) = idum
      	ran1  = min(am*iy,rnmx)
c     	ran1  = cos(ran1*1.e8)

      	return
      	end
