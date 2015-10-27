C> \ingroup selci
C> @{
      subroutine selci_wtconf(ifl,irefo,nn,indxci,mm)
*
* $Id: wtconf.f 24790 2013-11-11 18:56:49Z d3y133 $
*
      dimension irefo(nn),indxci(mm+1)
c
      write(1) irefo
      write(1) indxci
c
      end
C> @}
