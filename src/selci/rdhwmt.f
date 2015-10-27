C> \ingroup selci
C> @{
      subroutine selci_rdhwmt(iflwmt,mmulti, nsmax, nf, nfmax, nfmax2)
*
* $Id: rdhwmt.f 24790 2013-11-11 18:56:49Z d3y133 $
*
      dimension nf(0:32)
c
      call ifill(33,0,nf,1)
      read (iflwmt,*) mmulti, nsmax
      read (iflwmt,*) (nf(i),i=mod(nsmax,2),nsmax,2)
      nfmax = nf(nsmax)
      nfmax2 = nf(nsmax-2)
c
      end
C> @}
