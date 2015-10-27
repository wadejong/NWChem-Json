C> \ingroup selci
C> @{
C>
      integer function selci_idmin(n,a,ia)
*
* $Id: idmin.f 24790 2013-11-11 18:56:49Z d3y133 $
*
      real *8 a(ia,*)
      real *8 val
c
c     return index of minimum value in array a
c
      val = a(1,1)
      ind = 1
c
      do 10 i = 1,n
         if (a(1,i).lt.val) then
            val = a(1,i)
            ind = i
         endif
10    continue
c
      selci_idmin = ind
      end
C>
C> @}
