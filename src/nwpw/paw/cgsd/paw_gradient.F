c
c     $Id: paw_gradient.F 26433 2014-12-03 23:34:20Z mjacquelin $
c
      logical function paw_gradient(rtdb)
      implicit none
      integer rtdb
      logical  paw_minimizer,paw_stress
      external paw_minimizer,paw_stress

#include "bafdecls.fh"
#include "btdb.fh"

*     *** local variables ****
      logical includestress,value

      if (.not.btdb_get(rtdb,'includestress',mt_log,1,includestress))
     >   includestress = .false.

*     **** only calculate atom gradients ****
      if (.not.includestress) then
         value = paw_minimizer(rtdb,1)

*     **** include stress calculation ****
      else
         value = paw_stress(rtdb)
      end if

      paw_gradient = value
      return
      end

