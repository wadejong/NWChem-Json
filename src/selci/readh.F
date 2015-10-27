C> \ingroup selci
C> @{
      subroutine selci_readh(iflham,hh,nnci)
*
* $Id: readh.F 24790 2013-11-11 18:56:49Z d3y133 $
*
#include "implicit.fh"
      dimension hh(nnci)
c
      read (iflham) hh
c
      end
C> @}
