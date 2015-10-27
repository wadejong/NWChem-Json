C> \ingroup selci
C> @{
      subroutine selci_vadd(n,a,ia,b,ib,c,ic)
*
* $Id: vadd.f 24790 2013-11-11 18:56:49Z d3y133 $
*
      real*8 a(ia,*),b(ib,*),c(ic,*)
c
c     c(*) = b(*) + a(*)
C
      do 10 m = 1,n
         c(1,m) = b(1,m) + a(1,m)
 10   continue
c
      end
C> @}
