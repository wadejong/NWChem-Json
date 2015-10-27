C> \ingroup selci
C> @{
      subroutine selci_pconls(ioconf)
*
* $Id: pconls.F 24790 2013-11-11 18:56:49Z d3y133 $
*
#include "implicit.fh"
#include "ciinfo.fh"
      dimension ioconf(nintpo,noconf)
c
      write(ifllog,1)
 1    format(/' List of orbital occupations in the CI '/)
      do 10 iconf = 1,noconf
         call selci_prtcon(ifllog,norbs,ioconf(1,iconf),nintpo,nbitpi)
 10   continue
c
      end
C> @}
