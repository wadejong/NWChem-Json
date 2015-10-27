C> \ingroup selci
C> @{
      subroutine selci_prtcon(ifllog, norbs, ioconf, nintpo, nbitpi)
*
* $Id: prtcon.f 24790 2013-11-11 18:56:49Z d3y133 $
*
      dimension ioconf(nintpo),iocc(255)
c
      call selci_upkcon(norbs, iocc, ioconf, nintpo, nbitpi)
      call selci_wrtcon(ifllog, iocc, norbs)
c
      end
C> @}
