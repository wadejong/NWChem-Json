C> \ingroup selci
C> @{
      subroutine selci_rdhcon(iflcon, title, multi, nelec, issss, norbs,
     &     nnsmax, nci, noconf, nintpo, nbitpi, nbpsy, isym, nsym,
     &     inttyp, nsneed)
*
* $Id: rdhcon.f 24790 2013-11-11 18:56:49Z d3y133 $
*
      character*80 title
      dimension nbpsy(8), isym(255), nsneed(3)
c
c     read header of the ciconf file
c
      read(iflcon) title, multi, nelec, issss, norbs, nnsmax, 
     &     nci, noconf, nintpo, nbitpi, nbpsy, isym, nsym, inttyp,
     &     nsneed
c
      end
C> @}
