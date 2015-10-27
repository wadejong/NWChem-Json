      module json_nwchem

      use json_module

      implicit none

      type(json_value), pointer, save :: root_json, simulation
      type(json_value), pointer, save :: calculations
      type(json_value), pointer, save :: calcTask, calcResult
      type(json_value), pointer, save :: molProp
       
      integer, save :: taskCount, basCount, setupCount
      integer, save :: molIn, molOut

      logical, save :: basSame, molSame, setupSame
      logical, save :: jsonSubTask, printMolOrbs

      character(len=255), allocatable, save :: json_filename
      character(len=32), save :: moleculeID, basisSetID, setupID

      type json_value_pointer
        type(json_value), pointer :: jvp
      end type json_value_pointer

      type(json_value_pointer), save :: atomProp(1000)

      interface json_add_set
        module procedure json_add_setr
        module procedure json_add_seti
        module procedure json_add_seta
      end interface json_add_set

      contains

!   Start JSON structure and file 

      subroutine json_startup(filewrite,inputFile)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      character(len=*) :: filewrite, inputFile

      if (ga_nodeid().gt.0) return

      call json_initialize()
      call json_create_object(root_json,trim(filewrite))
      json_filename = filewrite
   
      call json_create_object(simulation, 'simulation')
      call json_add(root_json, simulation)

      call json_simulation_environment(inputFile)

      call json_create_array(calculations, 'calculations')
      call json_add(simulation, calculations)
      call json_write()

      taskCount = 0
      basCount = 0
      molIn = 0
      molOut = 0
      setupCount = 0
      basSame = .false.
      molSame = .false.
      setupSame = .false.
      jsonSubTask = .false.
      printMolOrbs = .true.

      end subroutine

! Close down JSON structure and file

      subroutine json_tidy()

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      if (ga_nodeid().gt.0) return

      call json_write()
      call json_destroy(root_json)
  
      end subroutine

! Write curren JSON data to file

      subroutine json_write()

      integer :: u

      open(newunit=u, file=json_filename, status='REPLACE')
      call json_print(root_json,u)
      close(u)

      end subroutine

! Add value plus units to JSON object

      subroutine json_add_setr(me, objectName, objectValue, objectUnits)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      character(len=*) :: objectname, objectUnits
      type(json_value), pointer :: me
      real(kind=8) :: objectValue

      type(json_value), pointer ::  val

      if (ga_nodeid().gt.0) return

      call json_create_object(val, trim(objectName))
      call json_add(me, val)
      call json_add(val, 'value', objectValue)
      call json_add(val, 'units', trim(objectUnits))
 
      end subroutine

! Add value plus units to JSON object

      subroutine json_add_seti(me, objectName, objectValue, objectUnits)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      character(len=*) :: objectname, objectUnits
      type(json_value), pointer :: me
      integer :: objectValue

      type(json_value), pointer ::  val

      if (ga_nodeid().gt.0) return

      call json_create_object(val, trim(objectName))
      call json_add(me, val)
      call json_add(val, 'value', objectValue)
      call json_add(val, 'units', trim(objectUnits))

      end subroutine

! Add set of values plus units to JSON object

      subroutine json_add_seta(me, objectName, objectValue, objectUnits)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      character(len=*) :: objectname, objectUnits
      type(json_value), pointer :: me
      real(kind=8), dimension(:) :: objectValue

      type(json_value), pointer ::  val

      if (ga_nodeid().gt.0) return

      call json_create_object(val, trim(objectName))
      call json_add(me, val)
      call json_add(val, 'value', objectValue)
      call json_add(val, 'units', trim(objectUnits))
 
      end subroutine

! Add simulation environment data to simulation

      subroutine json_simulation_environment(inputFile)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"
#include "stdio.fh"

      character(len=*) :: inputFile
      character(len=26) :: rundate
      character(len=80) :: temp80
      character(len=200) :: temp200
      character(len=:), allocatable :: inputString
      integer, parameter :: chunk_size=200
      integer :: nproc

      type(json_value), pointer :: simEnv

      if (ga_nodeid().gt.0) return

      call json_create_object(simEnv, 'simulationEnvironment')
      call json_add(simEnv, 'programRun', 'NWChem')
      call util_nwchem_version(temp200)
      call json_add(simEnv, 'programVersion', trim(temp200))
      call util_date(rundate)
      rundate(25:26) = ' '
      call json_add(simEnv, 'runDate', rundate)
      call util_hostname(temp80)
      call json_add(simEnv, 'hostMachine', trim(temp80))
      nproc = ga_nnodes()
      call json_add(simEnv, 'processorCount', nproc)
      call json_add(simEnv, 'inputFileName', inputFile)
      rewind(LuIn)
      inputString = ''
  1   read(LuIn, '(a)', end=3) temp200
        if (len(trim(temp200))>0) 
     &  inputString = inputString // trim(temp200) // ';'
      goto 1
  3   inputString(len(inputString):len(inputString)) = ' '
      call json_add(simEnv, 'inputFileData', trim(inputString))
      rewind(LuIn)
      if (allocated(inputString)) deallocate (inputString)
      call json_add(simulation,simEnv)

      end subroutine

! Add simulation time to simulation

      subroutine json_simulationTime(cpuTime, wallTime)
  
#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      real(kind=8) :: cpuTime, wallTime
      type(json_value), pointer :: timing_object
  
      if (ga_nodeid().gt.0) return

      call json_create_object(timing_object, 'simulationTime')
      call json_add(timing_object, 'cpuTime', cpuTime)
      call json_add(timing_object, 'wallTime', wallTime)
      call json_add(timing_object, 'units', 'second')
      call json_add(simulation, timing_object)
  
      end subroutine

! Add calculation time to object

      subroutine json_taskTime(cpuTime, wallTime)
  
#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      real(kind=8) :: cpuTime, wallTime
      type(json_value), pointer :: timing_object
  
      if (ga_nodeid().gt.0) return

      call json_create_object(timing_object, 'calculationTime')
      call json_add(timing_object, 'cpuTime', cpuTime)
      call json_add(timing_object, 'wallTime', wallTime)
      call json_add(timing_object, 'units', 'second')
      call json_add(calcTask, timing_object)

      end subroutine

! Start NWChem task

      subroutine json_start_task(calcType)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      real(kind=8) :: charge
      integer :: ispin, geom, basisin
      character(len=*) :: calcType

      character(len=8) :: istr1
      type(json_value), pointer :: pointTo
      logical :: found

      if (ga_nodeid().gt.0) return

      if (jsonSubTask) then
         call json_get(calcTask,'calculationResults',pointTo,found)
         if (found) call json_remove(pointTo, destroy=.true.)
         call json_create_object(calcResult, 'calculationResults')
         call json_add(calcTask,calcResult)
         return
      endif

      call json_create_object(calcTask, '')
      call json_add(calculations, calcTask)
      call json_add(calcTask, 'calculationType', calcType)
      if (.not.molSame) molIn = molIn + 1
      if (calcType=='geometryOptimization') then
         molOut = molIn + 1
      else 
         molOut = molIn
      endif 
      taskCount = taskCount + 1
      write(istr1,"(i4)") taskCount
      call json_add(calcTask, 'id',
     &              'calculation.'//trim(adjustl(istr1)))
      call json_create_object(calcResult, 'calculationResults')
      call json_add(calcTask,calcResult)

      end subroutine

! Calculation setup for NWChem task

      subroutine json_task_setup(geom,basisin,charge,ispin,wfntype,
     &                           movecs_in,nelec,theory)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      real(kind=8) :: charge
      integer :: ispin, geom, basisin, nelec
      character(len=*) :: wfntype, movecs_in, theory
      character(len=:), allocatable :: string
      character(len=8) :: idnum
      type(json_value), pointer :: calcSetup, pointTo
      logical :: found

      if (ga_nodeid().gt.0) return

      if (jsonSubTask) then
         call json_get(calcTask,'calculationSetup',pointTo,found)
         if (found) then
            call json_remove(pointTo, destroy=.true.)
            setupCount = setupCount - 1
         endif
      endif
      if (setupSame) then
         call json_add(calcTask, 'calculationSetup', setupID)
         return
      endif
      call json_create_object(calcSetup, 'calculationSetup')
      setupCount = setupCount + 1
      write(idnum,"(i4)") setupCount
      setupID = 'calculationSetup.'//trim(adjustl(idnum))
      call json_add(calcTask, calcSetup)
      call json_add(calcSetup, 'id', setupID)
      call json_add(calcSetup, 'molecularCharge', charge)
      call json_add(calcSetup, 'molecularSpinMultiplicity', ispin)
      call json_add(calcSetup, 'numberOfElectrons', nelec)
      call json_add(calcSetup, 'waveFunctionTheory',trim(theory))
      call json_add(calcSetup, 'waveFunctionType',trim(wfntype))
      call json_add(calcSetup, 'inputVectors',trim(movecs_in))
      call json_molecule(geom, calcSetup, molIn)
      if (theory/='Plane Wave Density Functional Theory') then
         call json_basis(basisin, calcSetup)
      endif
      if (theory=='Density Functional Theory')
     &    call json_add_functionals(calcSetup)
      call json_write()

      end subroutine

! Add DFT functionals

      subroutine json_add_functionals(me)

#include "../nwdft/include/cdft.fh"

      integer :: n
      character(len=9) :: local_c, nonlocal_c, nineb_c
      type(json_value), pointer :: funct, funcComp, me

      local_c = 'local    '
      nonlocal_c = 'non-local'
      nineb_c = '         '

      call json_create_array(funct,'exchangeCorrelationFunctional')
      do n = 1, numfunc
         if (xccomb(n)) call json_add(funct,'xcName', xcname(n))
      enddo
      call json_create_object(funcComp,'')
      if (lxfac(1).or.nlxfac(1)) then
         call json_create_object(funcComp,'')
         call json_add(funcComp, 'exchangeTermName',
     &                 trim(adjustl(xname(1))))
         call json_add(funcComp, 'exchangeTermFactor',xfac(1))
         call json_add(funct,funcComp)
      endif
      do n = 2, numfunc
         if (lxfac(n).and.nlxfac(n))then
         call json_create_object(funcComp,'')
         call json_add(funcComp, 'exchangeTermName',
     &                 trim(adjustl(xname(n))))
         call json_add(funcComp, 'exchangeTermFactor',xfac(n))
         call json_add(funct,funcComp)
         elseif (lxfac(n).and.(.not.nlxfac(n)))then
         call json_create_object(funcComp,'')
         call json_add(funcComp, 'exchangeTermName',
     &                 trim(adjustl(xname(n))))
         call json_add(funcComp, 'exchangeTermFactor',xfac(n))
         call json_add(funcComp, 'exchangeTermLocality',local_c)
         call json_add(funct,funcComp)
         elseif ((.not.lxfac(n)).and.nlxfac(n))then
         call json_create_object(funcComp,'')
         call json_add(funcComp, 'exchangeTermName',
     &                 trim(adjustl(xname(n))))
         call json_add(funcComp, 'exchangeTermFactor',xfac(n))
         call json_add(funcComp, 'exchangeTermLocality',nonlocal_c)
         call json_add(funct,funcComp)
         endif
      enddo

      do n = 1, numfunc
        if (cfac(n).gt.0.0d0) then
         if (lcfac(n).and.nlcfac(n))then
         call json_create_object(funcComp,'')
         call json_add(funcComp, 'correlationTermName',
     &                 trim(adjustl(cname(n))))
         call json_add(funcComp, 'correlationTermFactor',cfac(n))
         call json_add(funct,funcComp)
         elseif (lcfac(n).and.(.not.nlcfac(n)))then
         call json_create_object(funcComp,'')
         call json_add(funcComp, 'correlationTermName',
     &                 trim(adjustl(cname(n))))
         call json_add(funcComp, 'correlationTermFactor',cfac(n))
         call json_add(funcComp, 'correlationTermLocality',local_c)
         call json_add(funct,funcComp)
         elseif ((.not.lcfac(n)).and.nlcfac(n))then
         call json_create_object(funcComp,'')
         call json_add(funcComp, 'correlationTermName',
     &                 trim(adjustl(cname(n))))
         call json_add(funcComp, 'correlationTermFactor',cfac(n))
         call json_add(funcComp, 'correlationTermLocality',nonlocal_c)
         call json_add(funct,funcComp)
         endif
        endif
      enddo

      call json_add(me, funct)

      end subroutine

! End NWChem task

      subroutine json_end_task(cputime,walltime)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      real(kind=8) :: cputime,walltime

      if (ga_nodeid().gt.0) return

      if (jsonSubTask) return
      molIn = molOut
      call json_taskTime(cputime,walltime)
      call json_write()

      end subroutine

! Add molecule to object

      subroutine json_molecule(geom,me,input)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"
#include "nwc_const.fh"
#include "geom.fh"
#include "geomP.fh"
#include "sym.fh"

      integer :: geom
      integer, optional :: input
      type(json_value), pointer :: me

      integer :: atn
      character(len=16) :: elem
      character(len=2) :: elsym
      character(len=8) :: systemType, grpName
  
      integer, parameter :: maxireps=20
      integer :: nop, nir, nat, i, j, ij
      integer :: class_dim(maxireps)
      character(len=8) :: zir(maxireps), zclass(maxireps)
      character(len=8) :: imnum, ianum
      character(len=32) :: atomID
      character(len=10) :: units
      real(kind=8) :: chars(maxireps,maxireps)
      real(kind=8), dimension(9) :: lVec
      real(kind=8) :: scaleU

      type(json_value), pointer :: moleculeObj, atomsObj, atomObj
      type(json_value), pointer :: molSym, coordsJS, atPosObj
      type(json_value), pointer :: pointTo
      logical :: found
  
      if (ga_nodeid().gt.0) return
 
      if (.not. geom_check_handle(geom,'geom_print_json')) return
  
      if (molSame) then
         call json_add(me, 'molecule', moleculeID)
         return
      endif

      call json_get(me, 'molecule', pointTo, found)
      if (found) call json_remove(pointTo, destroy=.true.)

      systemType = 'molecule'
      if (isystype(geom).eq.3) systemType = 'crystal '
      call json_create_object(moleculeObj, systemType)
      if (present(input)) then
         write(imnum,"(i8)") input 
      else
         write(imnum,"(i8)") molOut
      endif
      moleculeID = 'Molecule.'//trim(adjustl(imnum))
      call json_add(moleculeObj, 'id', trim(moleculeID))
  
      scaleU = 1.0d0
      if (user_units(geom) .eq. 'a.u.') then
         units = 'bohr'
      else if (user_units(geom) .eq. 'angstroms') then
         scaleU = angstrom_to_au
         units = 'angstrom'
      else if (user_units(geom) .eq. 'nanometer') then
         scaleU = angstrom_to_au * 10.0d0
         units = 'nanometer'
      else if (user_units(geom) .eq. 'picometer') then
         scaleU = angstrom_to_au * 0.01d0
         units = 'picometer'
      endif

      call json_create_array(atomsObj, 'atoms')
      call json_add(moleculeObj, atomsObj)
      nat = ncenter(geom)
      do i=1,nat
         call json_create_object(atomObj, '')
         call json_add(atomsObj, atomObj)
         if (.not.geom_tag_to_element(tags(i,geom),elsym,elem,atn)) 
     &      call errquit('geom_print_json: tag not atom', 0, 911)
         write(ianum,"(i8)") i
         atomID = trim(tags(i,geom))//'.'//trim(adjustl(ianum))//
     &                              '.Mol'//trim(adjustl(imnum))
         call json_add(atomObj, 'id', trim(atomID))
         call json_add(atomObj, 'elementLabel', trim(tags(i,geom)))
         call json_add(atomObj, 'elementSymbol', elsym)
         call json_add(atomObj, 'elementNumber', atn)
         call json_add(atomObj, 'elementName', elem)
         call json_create_object(atPosObj, 'cartesianCoordinates')
         call json_add(atomObj,atPosObj)
         call json_create_array(coordsJS, 'value')
         call json_add(atPosObj,coordsJS)
         do j = 1, 3
            call json_add(coordsJS, '', coords(j,i,geom)/scaleU)
         enddo
         call json_add(atPosObj, 'units', trim(units) )
      enddo
   
! If periodic, add lattice vectors

      if (isystype(geom).eq.3) then
         ij = 1
         do i = 1, 3
            do j = 1, 3
               lVec(ij) = amatrix(i,j,geom)
               ij = ij + 1
            enddo
         enddo
         call json_add(moleculeObj, 'latticeVectors', lVec)
      end if

! Add symmetry information

      call json_create_object(molSym, 'symmetry')
      grpName = group_name(geom)
      call json_add(molSym, 'groupName', trim(adjustl(grpName)))
      if (.not.sym_char_table(grpName,nop,nir,class_dim, 
     &  zir,zclass,chars)) call errquit 
     &  ('sym_irrepname: failed to get character table',0,0)
      call json_add(molSym, 'groupIrrepName',zir(1:nir),trim_str=.true.)
      call json_add(molSym, 'groupClassName', zclass(1:nir), 
     &              trim_str=.true.)
      call json_add(moleculeObj, molSym)

! Write the molecule object

      call json_add(me,moleculeObj)
      call json_write()

      molSame = .true.
  
      end subroutine

! Add basis set to object

      subroutine json_basis(basin, me)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"
#include "nwc_const.fh"
#include "bas.fh"
#include "basP.fh"
#include "basdeclsP.fh"
#include "ecpso_decP.fh"
#include "ecpso_sfnP.fh"
#include "stdio.fh"
#include "util.fh"

      integer :: basin
      type(json_value), pointer :: me
  
      integer :: ecpid, soid
      type(json_value), pointer :: basisSet, pieces
      character(len=8) :: idnum
  
      if (ga_nodeid().gt.0) return

      if (basSame) then
         call json_add(me, 'basisSet', basisSetID)
         return
      endif
      call json_create_object(basisSet, 'basisSet')
      basCount = basCount+1
      write(idnum,"(i4)") basCount
      basisSetID = 'BasisSet.'//trim(adjustl(idnum))
      call json_add(basisSet, 'id', basisSetId)
      call json_create_array(pieces, 'basisFunctions')
      call json_basis_piece(basin,Is_ECP_in(basin),.false.,.false.,
     &                      pieces)
      if (Is_ECP_in(basin)) then
         if (.not. bas_get_ecp_handle(basin,ecpid)) call errquit 
     &      ('json_basis: fatal error',911, BASIS_ERR)
         call json_basis_piece(ecpid, Is_ECP_in(basin),.true.,.false.,
     &                         pieces)
      endif
      if (Is_SO_in(basin)) then
         if (.not. bas_get_ecp_handle(basin,soid)) call errquit 
     &      ('json_basis: fatal error',911, BASIS_ERR)
         call json_basis_piece(soid, Is_ECP_in(basin),.false.,.true.,
     &                         pieces)
      endif
      call json_add(basisSet, pieces)
      call json_add(me, basisSet)

      basSame = .true.
  
      end subroutine

! Actual adding of basis set component to object

      subroutine json_basis_piece(basisin,Has_ecp,oIs_ecp,oIs_so,tome)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"
#include "nwc_const.fh"
#include "geom.fh"
#include "basdeclsP.fh"
#include "bas.fh"
#include "basP.fh"
#include "ecpso_decP.fh"
#include "ecpso_sfnP.fh"
#include "bas_exndcf_dec.fh"
#include "bas_exndcf_sfn.fh"
#include "inp.fh"
#include "stdio.fh"
#include "util.fh"

      integer :: basisin, basis, ntags, itag, ncont, icont, fcont, lcont
      integer :: nelec, i, j, mylen, atn, cptr, itype
      integer :: nprim, iprim, ngen, igen, iexp, icoef, irexp, bptr
      logical :: oIs_ecp, oIs_so, Has_ecp
      character(len=9) :: myspcart
      character(len=2) :: symbol
      character(len=4) :: tagextend
      character(len=16) :: mytag, element
      character(len=64) :: myname
      character(len=20) :: mytype
      character(len=6) :: mymo
      character(len=2) :: orbType(-1:6), oType
      character(len=3) :: ecpType(-1:6), eType
      real(kind=8),allocatable :: bdata(:)
  
      type(json_value), pointer :: basExp, basCoef, basContS, tome
      type(json_value), pointer :: basRExp, basCont, basAtom
  
      orbType = (/ 'sp','s ','p ','d ','f ','g ','h ','i ' /)
      ecpType = (/ 'U L','U-s','U-p','U-d','U-f','U-g','U-h','U-i' /)
  
  
      basis = basisin + Basis_Handle_Offset
  
      if (bas_spherical(basis)) then
         myspcart = 'spherical'
      else
         myspcart = 'cartesian'
      endif
  
      mytype = 'orbitalBasis'
      tagextend = '-orb'
      if (oIs_ecp) then
         mytype = 'ecpPotential'
         tagextend = '-ecp'
      endif
      if (oIs_so) then
         mytype = 'spinorbitPotential'
         tagextend = '-so '
      endif
  
      ntags = infbs_head(Head_Ntags,basis)
  
      do itag = 1,ntags
        call json_create_object(basAtom, '')
        call json_create_array(basContS, 'basisSetContraction')
        mytag = bs_tags(itag,basis)
        myname = bs_stdname(itag,basis)
        call json_add(basAtom, 'id', trim(mytag)//tagextend)
        if (.not. geom_tag_to_element(mytag, symbol, element, atn)) 
     &     call errquit('json_basis: tag not atom', 0, 911)
        call json_add(basAtom, 'elementLabel', trim(mytag))
        call json_add(basAtom, 'elementType', trim(symbol))
        call json_add(basAtom, 'basisSetType', trim(mytype))
        call json_add(basAtom, 'basissetName', trim(myname))
        call json_add(basAtom, 'basisSetHarmonicType', trim(myspcart))
        if (Has_ecp) then
          if (.not.ecp_get_num_elec(basisin,mytag,nelec)) 
     &      call errquit('bas_ecce_print_basis: fatal error:nelec',911,
     &      BASIS_ERR)
          if (nelec>0) call json_add(basAtom,'numberElectronsReplaced',
     &                               nelec)
        endif
        ncont = infbs_tags(Tag_Ncont,itag,basis)
        fcont = infbs_tags(Tag_Fcont,itag,basis)
        do icont = 1, ncont
          call json_create_object(basCont, '')
          itype = infbs_cont(Cont_Type,fcont,basis)
          write(mymo,"(i4)") icont
          call json_add(basCont, 'id', 
     &           trim(trim(mytag)//tagextend//'c'//trim(adjustl(mymo))))
          if (oIs_ecp.or.oIs_so) then
             eType = ecpType(itype)
             call json_add(basCont,'basisSetShell',trim(eType))
          else
             oType = orbType(itype)
             call json_add(basCont,'basisSetShell',trim(oType))
          endif
          nprim = infbs_cont(Cont_Nprim,fcont,basis) 
          ngen  = infbs_cont(Cont_Ngen,fcont,basis) 
          iexp  = infbs_cont(Cont_Iexp,fcont,basis) 
          icoef = infbs_cont(Cont_Icfp,fcont,basis) 
          allocate(bdata(nprim))
          if (oIs_ecp.or.oIs_so) then
            call json_create_array(basRExp, 'basisSetRExponent')
            irexp = infbs_cont(Cont_Irexp,icont,basis)
            do i=1,nprim
               bdata(i) = sf_exndcf((irexp+i-1),basis)
            enddo
            call json_add(basCont, 'basisSetRExponent', bdata(1:nprim))
          endif
          do i=1,nprim
             bdata(i) = sf_exndcf((iexp+i-1),basis)
          enddo
          call json_add(basCont, 'basisSetExponent', bdata(1:nprim))
          do i=1,nprim
             bdata(i) = sf_exndcf((icoef+i-1),basis)
          enddo
          call json_add(basCont, 'basisSetCoefficient', bdata(1:nprim))
          deallocate(bdata)
          fcont = fcont + 1
          call json_add(basConts, basCont)
        enddo
        call json_add(basAtom ,basConts)
        call json_add(tome, basAtom)
      enddo
  
      end subroutine

! Add molecular orbitals to object

      subroutine json_orbitals(me, nbf, nmo, eval, occ, sym, 
     &                         g_movecs, geom, basis, nsets, myid)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"
#include "inp.fh"
#include "sym.fh"

      integer :: nbf, nmo, geom, basis, nsets
      integer, dimension(nmo,nsets) :: sym
      integer, dimension(nsets) :: g_movecs
      real(kind=8), dimension(nmo,nsets) :: eval, occ
      character(len=255) :: myid
      type(json_value), pointer :: me
 
      integer, parameter :: maxireps = 20
      integer :: nop, nir, i ,j
      integer, dimension (maxireps) :: class_dim
      character(len=8), dimension (maxireps) :: zir, zclass
      character(len=20) :: zname
      real(kind=8), dimension(maxireps*maxireps) :: chars
 
      real(kind=8), dimension(:,:,:), allocatable :: k_v
      character(len=16), dimension(:), allocatable :: k_ang
      character(len=8) :: ksym,istr1,jstr1,imnum

      type(json_value), pointer :: molOrbS, molOrb, oneOrb
      
      if (ga_nodeid().gt.0) return

      allocate(k_v(nbf,nmo,nsets))
      do i=1,nsets
         call ga_get(g_movecs(i),1,nbf,1,nmo,k_v(1,1,i),nbf)
      enddo

      allocate(k_ang(nbf))
      call bas_vec_info(basis, k_ang)
 
      call sym_group_name(geom, zname)
      if (.not. sym_char_table(zname, nop, nir, class_dim, 
     &           zir, zclass, chars)) call errquit 
     &           ('json_orbitals: no char table available ', 
     &           geom, 112)

      call json_create_object(molOrbs, 'molecularOrbitals')
      call json_add(molOrbs, 'id', myid)
      call json_add(molOrbs, 'atomicOrbitalDescriptions', k_ang(1:nbf),
     &              trim_str=.true.)
      
      call json_create_array(molOrb, 'molecularOrbital')

      istr1 = ''
      if (nsets>1) istr1 = '-alpha'
      write(imnum,"(i8)") molOut
      do i = 1, nsets
         if (i>1) istr1 = '-beta'
         do j = 1, nmo
            call json_create_object(oneOrb, '')
            write(jstr1,"(i8)") j
            call json_add(oneOrb, 'id', 'MolecularOrbital'//trim(istr1)
     &        //'.'//trim(adjustl(jstr1))//'.Mol'//trim(adjustl(imnum)))
            call json_add_set(oneOrb, 'orbitalEnergy', eval(j,i),
     &                        'Hartree')
            ksym = zir(sym(j,i))
            call json_add(oneOrb, 'orbitalSymmetry', trim(ksym))
            call json_add(oneOrb, 'orbitalOccupancy', occ(j,i))
            call json_add(oneOrb, 'moCoefficients', k_v(1:nbf,j,i))
            call json_add(molOrb,oneOrb)
         enddo
      enddo
      call json_add(molOrbs, molOrb)
      call json_add(me, molOrbs)
 
      deallocate(k_ang)
      deallocate(k_v)
  
      end subroutine

! Add calculated normal modes to result object

      subroutine json_freq(eigVal, eigVec, intens, icount, linear)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      integer :: icount
      logical, optional :: linear
      real(kind=8) :: eigval(icount), eigVec(icount,icount)
      real(kind=8) :: intens(icount,4)

      integer :: i, j, istart
      character(len=8) :: idnum

      type(json_value), pointer :: molModS, molMod, ZPE

      if (ga_nodeid().gt.0) return

      call json_create_array(molMods, 'normalModes')
      call json_add(calcResult,molMods)
      istart = 7
      if (linear) istart = 6
      do i = istart, icount
         call json_create_object(molMod, '')
         call json_add(molModS,molMod)
         write(idnum,"(i4)") i-istart+1
         call json_add(molMod,'id','normalMode.'//trim(adjustl(idnum)))
         call json_add_set(molMod,'normalModeFrequency', eigval(i),
     &                     'cm-1')
         call json_add_set(molMod,'normalModeInfraRedIntensity',
     &                     intens(i,2), '(debye/angs)**2')
         call json_add_set(molMod,'normalModeVector',
     &                     eigVec(1:icount,i), 'none')
      enddo

      end subroutine

! Startup for property calculations
!
! Note, because it is a linked list of pointers cannot deallocate 
! or call routine twice right now. Needs fixing.

      subroutine json_start_prop(geom)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"
#include "nwc_const.fh"
#include "geom.fh"
#include "geomP.fh"
#include "sym.fh"

      integer :: geom
  
      logical :: status

      integer :: iat, natom
      character(len=32) :: atomID
      character(len=8) :: imnum, ianum
      
      if (ga_nodeid().gt.0) return

      status=geom_ncent(geom,natom)
      call json_create_array(molProp, 'molecularProperties')
      call json_add(calcResult, molProp)
      write(imnum,"(i8)") molOut
      do iat = 1, natom
         call json_create_object(atomProp(iat)%jvp, '')
         call json_add(molProp, atomProp(iat)%jvp)
         write(ianum,"(i8)") iat
         atomID = trim(tags(iat,geom))//'.'//trim(adjustl(ianum))//
     &                                '.Mol'//trim(adjustl(imnum))
         call json_add(atomProp(iat)%jvp, 'propertiesOfAtom', 
     &                 trim(atomID))
      enddo

      end subroutine

! Add EFG tensor to property block

      subroutine json_add_efg(iat,ten,vec,eig,eta)
  
#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      integer :: iat
      real(kind=8) :: ten(9),vec(9),eig(3),eta

      type(json_value), pointer :: efgDat

      if (ga_nodeid().gt.0) return

      call json_create_object(efgDat, 'electricFieldGradient')
      call json_add(atomProp(iat)%jvp, efgDat)
      call json_add_set(efgDat, 'efgTensor', ten, 'atomic units')
      call json_add_set(efgDat, 'efgPrincipalComponents', eig, 
     &                  'atomic units')
      call json_add_set(efgDat, 'efgProjectionVectors', vec, 'none')
      call json_add_set(efgDat, 'efgAsymmetry', eta, 'none')

      end subroutine

! Add NMR tensor to property block

      subroutine json_add_nmr(iat,ten,dia,eigen,vec,eta,iso)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      integer :: iat
      real(kind=8) :: ten(9),dia(9),vec(9),eigen(3),eta,iso

      type(json_value), pointer :: nmrDat

      if (ga_nodeid().gt.0) return

      call json_create_object(nmrDat, 'chemicalShielding')
      call json_add(atomProp(iat)%jvp, nmrDat)
      call json_add_set(nmrDat, 'shieldingTensor', ten, 'ppm')
      call json_add_set(nmrDat, 'diamagneticComponentShieldingTensor', 
     &                  dia, 'ppm')
      ten = ten - dia
      call json_add_set(nmrDat, 'paramagneticComponentShieldingTensor', 
     &                  ten, 'ppm')
      call json_add_set(nmrDat, 'shieldingPrincipalComponents', eigen,
     &                  'ppm')
      call json_add_set(nmrDat, 'shieldingProjectionVectors', vec,
     &                  'none')
      call json_add_set(nmrDat, 'shieldingAnisotropy', eta, 'none')
      call json_add_set(nmrDat, 'isotropicShielding', iso, 'ppm')
   
      end subroutine

! Add Spin-Dipole tensor to property block

      subroutine json_add_spindip(iat,ten,vec,eig,iso)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      integer :: iat, iso
      real(kind=8) :: ten(9),vec(9),eig(3)

      type(json_value), pointer :: sdDat

      if (ga_nodeid().gt.0) return 

      call json_create_object(sdDat, 'spinDipole')
      call json_add(atomProp(iat)%jvp, sdDat)
      call json_add_set(sdDat, 'atomIsotope', iso, 'none')
      call json_add_set(sdDat, 'spinDipoleTensor', ten, 'MHz')
      call json_add_set(sdDat, 'spinDipolePrincipalComponents', eig,
     &                  'MHz')
      call json_add_set(sdDat, 'spinDipoleProjectionVectors', vec, 
     &                  'none')

      end subroutine

! Add Spin-Spin coupling tensor to property block

      subroutine json_add_spinspin(geom,iat,iso,jat,jso,ten,isotrop)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"
#include "nwc_const.fh"
#include "geom.fh"
#include "geomP.fh"
#include "sym.fh"

      integer :: geom, iat, jat, iso, jso
      real(kind=8) :: ten(9), isotrop

      if (ga_nodeid().gt.0) return

      end subroutine

! Add value property to property block

      subroutine json_prop_val_add(iat,propName,propValue,units)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      integer :: iat
      character(len=*) :: propName
      character(len=*), optional :: units
      character(len=32) :: useunits
      real(kind=8) :: propValue
      
      if (ga_nodeid().gt.0) return

      useunits = 'none'
      if (present(units)) useunits = units
      call json_add_set(atomProp(iat)%jvp,propName,propValue,useunits)

      end subroutine

! Add array property to property block

      subroutine json_prop_array_add(iat,propName,propValue,units)

#include "errquit.fh"
#include "mafdecls.fh"
#include "global.fh"

      integer :: iat
      character(len=*) :: propName
      character(len=*), optional :: units
      character(len=32) :: useunits
      real(kind=8) :: propValue(:)
      
      if (ga_nodeid().gt.0) return

      useunits = 'none'
      if (present(units)) useunits = units
      call json_add_set(atomProp(iat)%jvp,propName,propValue,useunits)
      
      end subroutine

      end module
