C> \ingroup selci
C> @{
      subroutine selci_rdconf(iflcon,ioconf,indxci,nintpo,noconf)
*
* $Id: rdconf.f 24790 2013-11-11 18:56:49Z d3y133 $
*
      integer ioconf(nintpo*noconf),indxci(noconf+1)
c
c     read occupations and index vector from ciconf file
c
      read (iflcon) ioconf
      read (iflcon) indxci
c
      end
C> @}
