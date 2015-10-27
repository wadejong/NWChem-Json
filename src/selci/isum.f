C> \ingroup selci
C> @{
      integer function selci_isum(n,m,im)
*
* $Id: isum.f 24790 2013-11-11 18:56:49Z d3y133 $
*
      dimension m(im,*)
c
c     return sum of integer array
c
      is = 0
      do 10 i = 1,n
         is = is + m(1,i)
 10   continue
c
      selci_isum = is
c
      end
C> @}
