C> \file selci_icopy.f
C> Integer copy
C>
C> \ingroup selci
C> @{
C>
      subroutine selci_icopy(n,x,ix,y,iy)
      implicit integer (a-z)
*
* $Id: selci_icopy.f 24783 2013-11-08 03:14:31Z d3y133 $
*
      dimension x(*),y(*)
c
      ixx = 1
      iyy = 1
      do 10 i = 1,n
         y(iyy) = x(ixx)
         ixx = ixx + ix
         iyy = iyy + iy
 10   continue
c
      return
      end
C>
C> @}
