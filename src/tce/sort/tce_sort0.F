      subroutine tce_sort_0(unsorted,sorted,factor)
c
c $Id: tce_sort0.F 26866 2015-02-22 04:36:22Z jhammond $
c
      implicit none
      double precision sorted
      double precision unsorted
      double precision factor
      sorted = unsorted * factor
      return
      end
