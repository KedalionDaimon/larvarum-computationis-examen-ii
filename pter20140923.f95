! THIS VERSION IS EXACTLY THE SAME AS *23_ana, BUT WITHOUT DEBUG INFO.
! IT SEEMS THE SYSTEM BEHAVES STRANGELY WHEN ONLY ONE WORD IS SAID.

! CHANGED FUNCTIONS FOR ANA:
! triangulate
! forget
! elimdoubles
! filteratoms

! ---------- PARAMETRISATION ------------------------------------------
! "Re-sliding" (i.e. the process of re-considering input in order to
! reach farther conclusions) is governed by "reslider" - set it to "1"
! to turn it off.
!
! The "snowflake mechanism" (i.e. continuing triangulation in the
! "aura" of a triangle, and trying to triangulate further from each
! side on) is governed by "snow" - set it to "0" to turn it off.
!
! The function "triangulate" contains the "globallimit"-variable which
! governs how many triangles one triangle side is allowed to conclude.
! ---------- END OF PARAMETRISATION -----------------------------------


! CHECK WHETHER PLANNING WOULD WORK IF THIS WERE TO BE OPERATED
! WITH ANALOGIES AS WELL. - I GUESS IT SHOULD WORK, AS IT OPERATES
! ONLY ON POSITIVE ATOMS, I.E. VIC-ATOMS. I CANNOT EVEN "MATCH"
! AN ANA-COMBINATION AT THIS STAGE. (HYPOTHESIS)

! The "global limit" for reasoning - i.e. where no further
! conclusions are regarded - has been implanted into triangulate.

! Radically shortening the input helped improve reasoning.
! Otherwise it was just overwriting the entire knowledge
! during reasoning - and knowledge was being destroyed.

! The purpose of this problem is to attempt triangulation
! in Fortran without using "atom values" - it is a purely
! "directional" and "positional" system, i.e. A-B is not
! the same as B-A, and the degree of influence of an atom
! on the further reasoning is determined by how far to the
! "front" (lower numbers) of the list of all atoms it is.

program pter
implicit none

integer, parameter :: sizeofall = 100000
integer, parameter :: l = 1
integer, parameter :: r = 2
integer, parameter :: sym = 3
integer, parameter :: iosize = 127
integer :: arrayofallatoms(sizeofall,3) ! the "list of all atoms"
integer :: soughtpair(2) ! that pair of input that we are considering
integer :: tripairs(sizeofall,2) ! the pairs of other triangle sides
integer :: sysinput(iosize)  ! the input from the outer world
integer :: sysanswer(iosize) ! the answer to the outer world
integer :: inputloop
integer :: tempsoughtpair(2)
integer :: temptripairs(sizeofall,2)
integer :: tempconcdepth
integer :: concdepth ! "depth" of conclusions, i.e. how many were made
integer :: highatom ! the super-atom of the sought pair
integer :: reslideinput(iosize)
! Setting the reslider to 1 disables re-sliding (see below):
integer, parameter :: reslider = 2 ! = iosize
integer :: sledgeloop
! Setting the snowflake to 0 escapes the indirect reasoning mechanism:
integer, parameter :: snow = 0
integer :: snowflake

! I/O SECTION.
! Files this operates on:
! syswords.txt
! sysinput.txt
! sysanswer.txt
! sysarray.txt
! TAKEN FROM LARCOM C FOR FILE PARSING:
integer :: wordwriter
! needed to write the wordlist into syswords.txt
integer :: wordreader
! reads out syswords.txt
integer, parameter :: abovedelimitersigns = 49
! used to load words without
! delimiter signs.
integer :: inputonechar
! loop to charge the input array
integer, parameter :: maxinlen = 2048
! maximum allowed characters of input,
! change as required.
integer :: inarray(maxinlen)
! the input array, with words in numeric form -
! must receive transformed results from primordial parsedarray parsing
character(LEN=maxinlen) :: inputstring
! the input in original string form
integer, parameter :: wordlistlength = 8190
! max. known elementary words
! FOLLOWING THE ABOVE, PLACE INTO AN INITIAL SYSARRAY.TXT:
! 0 0 8193
integer, parameter :: wordsize = 32
! max. noticeable length of each word
character(LEN=wordsize) :: wordlist(wordlistlength)
! list of known words
integer, parameter :: parsedarraylength = 2048
! max. words that could be parsed at once, relates to maxinlen
integer :: parsedarray(parsedarraylength)
! result of the first parsing of
! words from the inputstring into numeric representation of the words
integer :: parsedarrayposition
! the position within parsedarray currently
! subject to analysis
integer :: lengthofinput ! character length of input
integer, parameter :: unknownword = wordlistlength
! designator of unparseable word due to reaching the limit of
! knowable words - this cannot be 0, as that means end of string.
character(LEN=wordsize), parameter :: notknownword = "UNKNOWN"
integer, parameter :: maxoutputlen = maxinlen
! the maximum output length
character(LEN=maxoutputlen) :: outputstring
! the string given as answer
! to the input of the user and that is saved in sysanswer.txt
integer, parameter :: planarraylength = parsedarraylength
! length of the
! plan - in accordance with how many words can be parsed at once
integer :: planarray(planarraylength)
! the array containing the "plan",
! which is where the answer to be given to the user is composed, after
! adaption of detailedplan so that planarray can be translated dirctly
! into a string
integer :: nextwordposition ! textual position of the next word
integer :: chargelofall ! reading and
integer :: dumplofall   ! writing the list of all atoms
character(LEN=30) :: FORMAT
! END OF TAKEN FROM LARCOM C FOR FILE PARSING.
integer :: symnumber

FORMAT = "(26I5)"

! Charge the array of all atoms ere reading it from a file.
arrayofallatoms(:,:2) = 0
do symnumber = 1, sizeofall
  arrayofallatoms(symnumber,3) = symnumber + wordlistlength + 2
  ! This " + 2" is a buffer, assuming the wordlistlength is 8192.
end do

! ---------- LOAD THE OTHER VALUES NEEDED FOR FURTHER PROCESSING ------

inputstring = ""

open (unit=10, file="sysarray.txt", status="old", action="read")
do chargelofall = 1, sizeofall
  read (10,*, END = 100) arrayofallatoms(chargelofall,:)
end do
100 close (10)

open (file="sysinput.txt", unit=11, access="stream", &
&  form="unformatted", action="read")
do inputonechar = 1, maxinlen
  read (11, END = 110) inputstring(inputonechar:inputonechar)
end do
110 close (11)

! print *, "input = ", inputstring(:)

! ---------- CHARGE THE WORDLIST --------------------------------------

! inputstring has been supplied already
lengthofinput = inputonechar

wordlist(:) = ""

! Prior to analysis, load the words - but only them, not the signs!

open (unit = 12, file = "syswords.txt")
! Unit MUST NOT be 0,5,6,100,101,102, all other pos. integers are OK.
do wordreader = abovedelimitersigns, wordlistlength - 1
  read (12,*, END = 120) wordlist(wordreader)
end do
120 close (12)

wordlist(wordlistlength) = notknownword
parsedarray(:) = 0
parsedarrayposition = 1

call inreader(lengthofinput,parsedarrayposition,inputstring(:), &
& wordlist(:),parsedarray(:))

sysinput(:iosize) = parsedarray(:iosize)
! print *, "parsed array = ", parsedarray(:10)

! Write back to the parserwords the adjusted wordlist.
open (unit = 13, file = "syswords.txt")
do wordwriter = abovedelimitersigns, wordlistlength - 1
  write (13,*) wordlist(wordwriter)
end do
close (13)

! ---------- CONSIDER THE INPUT AND SELECT A TRIANGULATION ------------

! The list of all atoms is to be loaded from a file.
! It can be empty, but it MUST contain numbering on
! the third column - this numbering is important as
! the numbers are being "recycled" when creating new
! atoms etc. (That spares you a renumerate-function.)

! arrayofallatoms(1,:) = (/ 1, 2, 201 /)
! arrayofallatoms(2,:) = (/ 3, 13, 202 /)
! arrayofallatoms(3,:) = (/ 13, 6, 203 /)
! arrayofallatoms(4,:) = (/ 7, 8, 204 /)
! arrayofallatoms(5,:) = (/ 13, 10, 205 /)
! arrayofallatoms(6,:) = (/ 6, 12, 206 /)
! arrayofallatoms(7,:) = (/ 13, 14, 207 /)
! arrayofallatoms(8,:) = (/ 15, 6, 208 /)
! arrayofallatoms(9,:) = (/ 6, 13, 209 /)
! arrayofallatoms(10,:) = (/ 13, 20, 210 /)

! In the case of no knowledge, it creates
! atoms autonomously - if you want to start
! clean, then you may actually set it to:
!
! arrayofallatoms(:,:2) = 0
!
! and also create an appropriate third column.

! This is the input that the system has received -
! this is typically to be loaded from a file, too.
! sysinput(:) = (/ 14, 13, 6 /)
! print *, "sysinput = ", sysinput(:)

! print *, "before reasoning:"
! print *, "arrayofallatoms 1 = ", arrayofallatoms(:,1)
! print *, "arrayofallatoms 2 = ", arrayofallatoms(:,2)
! print *, "arrayofallatoms 3 = ", arrayofallatoms(:,3)

! "Re-sliding" means to re-consider input a number of times.
! This leads to deeper conclusions from otherwise the same input.
! If set to "1", then there is only one input consideration and
! the feature is effectively disabled (leading to less, but more
! "certain" conclusions). - This is like the re-hierarchisation
! mechanism of Larcom C.

! HERE IS THE RE-SLIDER
reslideinput = sysinput(:)
do sledgeloop = 1, reslider
sysinput(:) = reslideinput

! Now walk through the input from back to front and
! try to detemine which pair delivers the "best" triangulation.
! This is the one who creates the most conclusions in the tripairs.
! The tripairs express the triangulation results, whereby "triangulate"
! is the "classical" (direct) triangulation, whereas multiconc is
! "triangulation on the triangulation sides", i.e. the "snowflake
! mechanism" that greatly extends reasoning.

do ! This is the main hierarchisation loop - you
   ! continue to re-hierarchise, until one single atom is on the "top".
  tempconcdepth = -1 ! An impossible initial value, really it is min 0.
  ! For each hierarchisation stage, compute the triangulation of each
  ! possible input pair - CONSIDER MAKING SURE YOU DO NOT DO IT DOUBLE
  ! FOR ANYBODY. - Practically, nobody speaks in that many repetitions.
  do inputloop = iosize, 2, -1 ! max. length of input
    if (sysinput(inputloop) == 0) then
      cycle
    end if

    soughtpair(r) = sysinput(inputloop)
    soughtpair(l) = sysinput(inputloop - 1)

    ! tripairs(:,:) = 0 ! not needed, triangulate takes care of this.

    ! This is the part actually undertaking triangulation.
    tripairs(:,:) = triangulate(arrayofallatoms(:,:), soughtpair(:))
    ! Loop this for a number of times to gain wide "snowflake"
    ! reasoning (i.e. extra triangulation conclusions drawn from the
    ! first triangulation conclusions).
    if (snow > 0) then
      do snowflake = 1, snow
        tripairs(:,:) = multiconc(arrayofallatoms(:,:), tripairs(:,:))
      end do
    end if

    ! concdepth shows you how many conclusions were made by a pair -
    ! when zeroes are reached, there are no further conclusions.
    concdepth = minloc(tripairs(:,1), DIM=1, MASK= &
&   ((tripairs(:,1) == 0) .AND. (tripairs(:,2) == 0))) ! While one test
    ! should be enough, I prefer to do both for the sake of clarity.
    if (concdepth == 0) then
      tempconcdepth = iosize + 1 ! Max. depth possible, plus 1.
      tempsoughtpair(:) = soughtpair(:)
      temptripairs(:,:) = tripairs(:,:)
      exit ! No further pair can ever beat the max. conclusion depth.
    else if (concdepth > tempconcdepth) then
      tempconcdepth = concdepth
      tempsoughtpair(:) = soughtpair(:)
      temptripairs(:,:) = tripairs(:,:)
    end if
  end do
  ! As your "temporary" variables should now contain the "best"
  ! combination, go ahead and extract it:
  concdepth = tempconcdepth
  soughtpair(:) = tempsoughtpair(:)
  tripairs(:,:) = temptripairs
  ! Now, you have the "optimum triangulation" - you must apply it to
  ! the list of all atoms.

  ! The conclusions within tripairs - new as well as known ones - are
  ! now being merged into the list of all atoms, making sure there are
  ! no repetitions of atoms.
  arrayofallatoms(:,:) = &
& filteratoms(arrayofallatoms(:,:), tripairs(:,:))
  ! Now make absolutely sure that the sought pair is on the very front:
  ! eliminate any other tripairs with exception of the selected pair...
  tripairs(:,:) = 0
  tripairs(1,:) = soughtpair
  ! ... and "merge in" this single pair into the list of all atoms;
  ! (that extra step should normally not be necessary, if the array
  ! of all atoms is long enough - it SHOULD then be contained anyway).
  arrayofallatoms(:,:) = &
& filteratoms(arrayofallatoms(:,:), tripairs(:,:))
  highatom = arrayofallatoms(1,3) ! get the atom super-atom's number

  ! having the input, the sought pair and its super-atom, hierarchise:
  sysinput(:) = hierarchise(sysinput(:), soughtpair(:), highatom)

  ! Mind that this is only printing the input list up to position 10.
  ! print *, "next hierarchisation stage: ", sysinput(:10)

  if (sysinput(2) == 0) then
    exit ! i.e. all hierarchisation up to sysinput(1) is done.
  end if
end do
end do ! The reslider loop ends here.

! Now, with the top hierarchisation, you can try to find a plan:
sysanswer(:) = resultplan(arrayofallatoms(:,:), sysinput(1))

! print *, "plan = ", sysanswer(:)

! print *, "after reasoning:"
! print *, "arrayofallatoms 1 = ", arrayofallatoms(:,1)
! print *, "arrayofallatoms 2 = ", arrayofallatoms(:,2)
! print *, "arrayofallatoms 3 = ", arrayofallatoms(:,3)

! This is not needed, but just shall remind you of the direction:
! print *, "cshift: -1", cshift((/ 1, 2, 3, 4 /), SHIFT=-1)

! sysanswer(:) = (/50, 49, 47/)


! ---------- THE BELOW PART PARSES planarray(:) INTO outputstring -----
outputstring = ""
! ACTIVATE:
nextwordposition = 1
planarray(:) = 0
planarray(:iosize) = sysanswer(:iosize)
call outwriter(nextwordposition,outputstring,wordlist(:),planarray(:))
! outputstring = "OLD MACDONALD HAD A FARM" ! DEACTIVATE
! print *, "outputstring = ", outputstring

open (unit = 14, file = "sysanswer.txt")
  write (14,*) TRIM(outputstring)
close (14)

! ---------- WRITE THE OTHER VALUES NEEDED FOR FURTHER PROCESSING -----

open (unit=15, file="sysarray.txt")
do dumplofall = 1, sizeofall
! It is NOT till the elementarythreshold - as that is a VALUE,
! not a POSITION!
  write (15,*) arrayofallatoms(dumplofall,:)
end do
close (15)


contains
! Now come all the functions.


! That "pure integer" part can be enabled when you no longer print
! any intermediate results from inside the function. You cannot print
! in a pure function.
! pure integer 
function &
& forget(arrayofallatoms)
! This function removes the oldest atom that is not a sub-atom anywhere
! and recycles its atom number (i.e. puts it on the very front for use
! by the next atom).
implicit none
integer, parameter :: sizeofall = 100000
integer, parameter :: l = 1
integer, parameter :: r = 2
integer, parameter :: sym = 3
integer :: forget(sizeofall,3)
integer, intent(in) :: arrayofallatoms(sizeofall,3)
integer :: lastplace
integer :: countdown
integer :: usedasleft
integer :: usedasright

! Lastplace shall show you where to forget an atom.
lastplace = 0

! It is a countdown as you are looking for the oldest unused atom.
do countdown = sizeofall, 1, -1

  ! Test for usage as a sub-atom.
  usedasleft = minloc(arrayofallatoms(:,l), DIM=1, MASK= &
& (arrayofallatoms(:,l) == arrayofallatoms(countdown,sym)))

  usedasright = minloc(arrayofallatoms(:,r), DIM=1, MASK= &
& (arrayofallatoms(:,r) == arrayofallatoms(countdown,sym)))

  ! Now, also check for possible ana-connections,
  ! or more generally, "opposite connections":
  if ((usedasleft == 0) .AND. (usedasright == 0)) then
    usedasleft = minloc(arrayofallatoms(:,l), DIM=1, MASK= &
&   (arrayofallatoms(:,l) == -(arrayofallatoms(countdown,sym))))

    usedasright = minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   (arrayofallatoms(:,r) == -(arrayofallatoms(countdown,sym))))
  end if

  ! You got a valid lastplace if there is no other atom that uses
  ! the examined atom as a sub-atom, and also not its negative.
  if ((usedasleft == 0) .AND. (usedasright == 0)) then
    lastplace = countdown
    exit
  end if

end do

if ((lastplace /= 0) .AND. (lastplace /= sizeofall)) then
! Rotate the section that ends with the lastplace-position...

  forget(:lastplace,:) = &
& cshift(arrayofallatoms(:lastplace,:), SHIFT=-1)

  forget(1,l:r) = 0 ! Zerofy the sub-atoms, but not the super-atom.
  ! By re-cycling the atom number like this, I can avoid needing
  ! any function to "renumerate the atoms" - atom numbers will
  ! simply never "grow", and rather they will be recycled.

! print *, "forget preserves atom number: ", forget(1,3)

  forget(lastplace + 1:,:) = arrayofallatoms(lastplace + 1:,:)

else
! ... or otherwise simply rotate the entire list of all atoms.

  forget(:,:) = &
& cshift(arrayofallatoms(:,:), SHIFT=-1)
  forget(1,l:r) = 0 ! Zerofy the sub-atoms, but not the super-atom.

! print *, "forget preserves atom number: ", forget(1,3)

end if

return

end function forget


! pure integer
function &
& elimdoubles(arraywithdoubles, tripairs)
! This function merges tripairs into the array with doubles.
! There tripairs go to the front and eventual doubles are deleted.
! It is infact what was previously filteratoms, but it is for
! sizeofall,2 rather than for sizeofall,3-dimensioned arrays.
implicit none
integer, parameter :: sizeofall = 100000
integer, parameter :: l = 1
integer, parameter :: r = 2
integer :: elimdoubles(sizeofall,2)
integer :: zerovector(sizeofall,2)
integer, intent(in) :: arraywithdoubles(sizeofall,2)
integer, intent(in) :: tripairs(sizeofall,2)
integer :: placeseen
integer :: loopeach
integer :: maxloop

! maxloop shows you how far it is still sensible to evaluate tripairs.
maxloop = minloc(tripairs(:,l), DIM=1, MASK= &
& ((tripairs(:,l) == 0) .AND. (tripairs(:,r) == 0)))

! Test all, if tripairs is full:
if (maxloop == 0) then
  maxloop = sizeofall ! relating to tripairs
! Test none, if tripairs is empty:
else if (maxloop == 1) then
  elimdoubles(:,:) = arraywithdoubles(:,:)
  return
end if

elimdoubles(:,:) = arraywithdoubles(:,:)

! This loop relates to the max. tripairs, not the list of all atoms.
do loopeach = 1, maxloop - 1 ! concurrent (loopeach = 1:(maxloop - 1))
! Basically, you zerofy each of the doubles where there was a double.
! (This does not preserve the atom number. - This is the reason why
! filtering had to be re-implemented in a different way.)

  if ((tripairs(loopeach, l) == 0) .AND. &
&     (tripairs(loopeach, r) == 0)) then
    exit
  end if

  placeseen = minloc(arraywithdoubles(:,1), DIM=1, MASK= &
& ((arraywithdoubles(:,l) == tripairs(loopeach,l)) .AND. &
& (arraywithdoubles(:,r) == tripairs(loopeach,r))))

  if ((placeseen == 0) .AND. &
&    (tripairs(loopeach,l) < 0) .AND. (tripairs(loopeach,l) < 0)) then

    ! Give an analogy pair a second chance by looking for its reverse:
    placeseen = minloc(arraywithdoubles(:,1), DIM=1, MASK= &
&   ((arraywithdoubles(:,l) == tripairs(loopeach,r)) .AND. &
&   (arraywithdoubles(:,r) == tripairs(loopeach,l))))

  end if

  if (placeseen /= 0) then
!    print *, "zerofying: ", elimdoubles(placeseen,:)
    elimdoubles(placeseen,:) = 0
  end if

end do

! "zero vector" became necessary in order to cleanse elimdoubles
! from scads of array elements that remain in their original positons
! after "pack"-ing the array. - Your aim here is to "pack" all those
! elements of the elimdoubles array that are not zero (i.e. that have
! not been zerofied as "doubles" of the tripairs).
zerovector(:,:) = 0
! This operation means my elementary atoms cannot be combined with 0.
! Maybe make them as x = (x, -x)?
zerovector(:,l) = pack(elimdoubles(:,l), &
& MASK=(elimdoubles(:,l) /= 0)) ! VECTOR(1:10) = 0)
! Complained about syntax, consider implanting some "vector of zeroes".

zerovector(:,r) = pack(elimdoubles(:,r), &
& MASK=(elimdoubles(:,r) /= 0)) ! VECTOR(1:10) = 0)
! The VECTOR size is that of the array of all atoms.

elimdoubles(:,:) = zerovector(:,:)

return

end function elimdoubles


! pure integer
function &
& triangulate(arrayofallatoms, soughtpair)
! This version contains reasoning for analogies. It is a monstrous
! serial ivory tower of logic - I shall think how to parallelize it
! later on. In the meantime, of course you CAN use the purely vic-
! atom driven older function - it works perfectly fine.

! This function performs "batch" triangulation for a given pair, i.e.,
! not only one triangle is formed, but every possible triangle as long
! as it has not been "contradicted" (i.e. where the counter-conclusion
! is reached before the hypothetical conclusion we try to reach).
implicit none
integer, parameter :: sizeofall = 100000
integer, parameter :: l = 1
integer, parameter :: r = 2
integer :: triangulate(sizeofall, 2)
integer, intent(in) :: arrayofallatoms(sizeofall,3)
integer, intent(in) :: soughtpair(2)
! The global limit of conclusions. (Half a conclusion pair.)
! Set it to zero in order to disable it.
integer :: globallimit
integer :: A ! left atom of the sought pair
integer :: B ! right atom of the sought pair
integer :: X ! left atom of the pair in the list of all atoms
integer :: Y ! right atom of the pair in the list of all atoms
! The "conf"-elements are the left and the right atoms of
! congruent third pairs, whereas the "anti"-elements belong to the
! incongruent third pairs.
integer :: confA1
integer :: confB1
integer :: confA2
integer :: confB2
integer :: antiA1
integer :: antiB1
integer :: antiA2
integer :: antiB2
integer :: antiA3
integer :: antiB3
! The first atom minlocs.
integer :: primminloc
integer :: primminloc1
integer :: primminloc2
integer :: antiprimminloc
integer :: antiprimminloc1
integer :: antiprimminloc2
integer :: antiprimminloc3
! The second-atom and counter-second-atom minlocs.
integer :: secminloc
integer :: countersecminloc
integer :: countersecminloc1
integer :: countersecminloc2
integer :: countersecminloc3
! The affirmative or contrary minlocs - i.e. where
! the third pairs are seen in the list of all atoms.
integer :: confminloc
integer :: confminloc1
integer :: confminloc2
integer :: antiminloc
integer :: antiminloc1
integer :: antiminloc2
integer :: antiminloc3
! The actual atoms that will form the result
integer :: secondleft
integer :: secondright
integer :: thirdleft
integer :: thirdright
integer :: recordpair
integer :: gothroughatoms
integer :: switchprimA
integer :: switchprimB
integer :: searchlimit ! Stop searching for conclusions
! behind the contradiction of the first pair.

globallimit = 32
! if you want to have only two triangles, say:
! globallimit = 4 ! always double the desired count of triangles.

! THE TRIANGULATION TABLE LOOKS LIKE THIS:

! A B is always the sought atom, X Y always the connection
! encountered in the list of all atoms.

! A pair is vic if both atoms are above zero and ana
! if both atoms are below zero.

! A few definitions:
! "^" shall mean "is the same as"
! "~~>" shall mean "leads to the conclusions"
! "&" denotes a further confirmation or a further contradiction
! "vs." denotes the oppositions to the current conclusion
! "->" means a directional vic-connection
! "==" means an ana-connection

! Sub-atoms are negated in order to change their sign to one
! acceptable to the conclusion at hand. It is assumed that a
! vic-atom consists out of two positive atoms and that an
! ana-atom consists out of two negative atoms.

! A->B, X->Y
! A^X ~~> B==Y ~~>   -B -Y   &   -Y -B   vs.   B  Y   &   Y  B
! B^Y ~~> A==X ~~>   -A -X   &   -X -A   vs.   A  X   &   X  A
! A^Y ~~> X->B ~~>    X  B   vs.  B  X   &    -B -X   &  -X -B
! B^X ~~> A->Y ~~>    A  Y   vs.  Y  A   &    -A -Y   &  -Y -A

! A==B, X==Y
! A^X ~~> B==Y ~~>    B  Y   &    Y  B   vs.  -B -Y   &  -Y -B
! B^Y ~~> A==X ~~>    A  X   &    X  A   vs.  -A -X   &  -X -A
! A^Y ~~> X==B ~~>    X  B   &    B  X   vs.  -X -B   &  -B -X
! B^X ~~> A==Y ~~>    A  Y   &    Y  A   vs.  -A -Y   &  -Y -A

! A->B, X==Y:
! A^X ~~> Y->B ~~>   -Y  B   vs.  B -Y   &     Y -B   &  -B  Y
! B^Y ~~> A->X ~~>    A -X   vs. -X  A   &    -A  X   &   X -A
! A^Y ~~> X->B ~~>   -X  B   vs.  B -X   &     X -B   &  -B  X
! B^X ~~> A->Y ~~>    A -Y   vs. -Y  A   &    -A  Y   &   Y -A

! A==B, X->Y:
! A^X ~~> B->Y ~~>   -B  Y   vs.  Y -B   &     B -Y   &  -Y  B
! B^Y ~~> X->A ~~>    X -A   vs. -A  X   &    -X  A   &   A -X
! A^Y ~~> X->B ~~>    X -B   vs. -B  X   &    -X  B   &   B -X
! B^X ~~> A->Y ~~>   -A  Y   vs.  Y -A   &     A -Y   &  -Y  A

! THERE ARE GENERALLY TWO PATTERNS HERE IN THE CONSEQUENCES:
! id vs. contra1 contra2 contra3
! OR
! id co-id vs. contra1 contra2
! - IN EACH CASE, THEY WORK EXACTLY THE SAME, I.E. THERE ARE
! ONLY "TWO" CONSEQUENCES, EVEN IF THEY ARE STATED MANY TIMES.

triangulate(:,:) = 0
recordpair = 1

! Set the global limit of triangulation to maximum
! if it has been disabled.
if (globallimit == 0) then
  globallimit = sizeofall
end if

A = soughtpair(l)
B = soughtpair(r)

if ((A == 0) .AND. (B == 0)) then
  return ! There is no reasoning with a zero-pair
elseif (A == B) then
! This is a special case and further triangulation
! is impossible. However, it CAN be determined
! whether "rather this double has been seen"
! or whether "rather the counter-double has been seen".
! That is all the "triangulation" done here.

return

! This can be activated if I should ever decide
! that triangulation shall also return the original
! atom in the first position:

!   primminloc = &
! &   minloc(arrayofallatoms(:,l), DIM=1, MASK= &
! &   ((arrayofallatoms(:,l) == A) .AND. &
! &   (arrayofallatoms(:,r) == A)))

!   antiprimminloc = &
! &   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
! &   ((arrayofallatoms(:,l) == -A) .AND. &
! &   (arrayofallatoms(:,r) == -A)))

!   if (primminloc > antiprimminloc) then
!     return
!   else
!     triangulate(1,1) = A
!     triangulate(1,2) = A
!     return
!   end if

else if (((A > 0) .AND. (B < 0)) .OR. &
&        ((A < 0) .AND. (B > 0))) then
  return ! This case should never occur and reasoning
         ! has not been foreseen for it.
else if ((A > 0) .AND. (B > 0)) then
! A B is vic
! Determine whether the pair is seen first, or one of its three
! possible contradictions.
  primminloc = &
&   minloc(arrayofallatoms(:,l), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == A) .AND. &
&   (arrayofallatoms(:,r) == B)))

  antiprimminloc1 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == B) .AND. &
&   (arrayofallatoms(:,r) == A)))

  antiprimminloc2 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == -A) .AND. &
&   (arrayofallatoms(:,r) == -B)))

  antiprimminloc3 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == -B) .AND. &
&   (arrayofallatoms(:,r) == -A)))

  ! Determine the "best" antiprimminloc.
  antiprimminloc = antiprimminloc1

  if ((antiprimminloc2 /= 0) .AND. &
& (antiprimminloc2 < antiprimminloc)) then
    antiprimminloc = antiprimminloc2
  end if

  if ((antiprimminloc3 /= 0) .AND. &
& (antiprimminloc3 < antiprimminloc)) then
    antiprimminloc = antiprimminloc3
  end if

  ! No further triangulation can be undertaken if the counter-pair is
  ! found before the actual pair. A position of "0" means "not found".
  if (((primminloc > antiprimminloc) .AND. (antiprimminloc /= 0)) &
& .OR. &
& ((primminloc == 0) .AND. (antiprimminloc /= 0))) then
    return
  else
    searchlimit = antiprimminloc
  end if

  if (searchlimit == 0) then ! That means the pair has not been found.
    searchlimit = sizeofall ! Entire length.
  end if

else if ((A < 0) .AND. (B < 0)) then ! That actually could be an "else"
! A B is ana.
! Determine whether the pair is seen first, or one of its two
! possible contradictions.
  primminloc1 = &
&   minloc(arrayofallatoms(:,l), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == A) .AND. &
&   (arrayofallatoms(:,r) == B)))

  primminloc2 = &
&   minloc(arrayofallatoms(:,l), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == B) .AND. &
&   (arrayofallatoms(:,r) == A)))

  antiprimminloc1 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == -A) .AND. &
&   (arrayofallatoms(:,r) == -B)))

  antiprimminloc2 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == -B) .AND. &
&   (arrayofallatoms(:,r) == -A)))

  ! Determine the "best" primminloc.
  primminloc = primminloc1

  if ((primminloc2 /= 0) .AND. &
& (primminloc2 < primminloc)) then
    primminloc = primminloc2
    switchprimA = B ! That should never be
    switchprimB = A ! necessary, because
    A = switchprimA ! analogies should always have only 
    B = switchprimB ! ONE direction and so an "opposite"
  end if            ! should never be able to be exist.

  ! Determine the "best" antiprimminloc.
  antiprimminloc = antiprimminloc1

  if ((antiprimminloc2 /= 0) .AND. &
& (antiprimminloc2 < antiprimminloc)) then
    antiprimminloc = antiprimminloc2
  end if

  ! No further triangulation can be undertaken if the counter-pair is
  ! found before the actual pair. A position of "0" means "not found".
  if (((primminloc > antiprimminloc) .AND. (antiprimminloc /= 0)) &
& .OR. &
& ((primminloc == 0) .AND. (antiprimminloc /= 0))) then
    return
  else
    searchlimit = antiprimminloc
  end if

  if (searchlimit == 0) then ! That means the pair has not been found.
    searchlimit = sizeofall ! Entire length.
  end if
end if

! Huge bracing loop for analysing the list of all atoms.
do gothroughatoms = 1, sizeofall - 1 ! of the list of all atoms
X = arrayofallatoms(gothroughatoms, l)
Y = arrayofallatoms(gothroughatoms, r)

if ((X == 0) .AND. (Y == 0)) then
  return ! We reached the end of known atoms,
end if   ! no further reasoning is possible.

if (X == Y) then
  cycle ! There is no reasoning with a second pair consisting
        ! out of the same atoms
end if

if (((X == A) .AND. (Y == B)) .OR. &
&   ((X == B) .AND. (Y == A)) .OR. &
&   ((X == -A) .AND. (Y == -B)) .OR. &
&   ((X == -B) .AND. (Y == -A))) then
  cycle ! A pair does not reason with variants of its own self.
end if

confA1 = 0
confB1 = 0
confA2 = 0
confB2 = 0
antiA1 = 0
antiB1 = 0
antiA2 = 0
antiB2 = 0
antiA3 = 0
antiB3 = 0

primminloc = 0
primminloc1 = 0
primminloc2 = 0
antiprimminloc = 0
antiprimminloc1 = 0
antiprimminloc2 = 0
antiprimminloc3 = 0
secminloc = 0
countersecminloc = 0
countersecminloc1 = 0
countersecminloc2 = 0
countersecminloc3 = 0
confminloc = 0
confminloc1 = 0
confminloc2 = 0
antiminloc = 0
antiminloc1 = 0
antiminloc2 = 0
antiminloc3 = 0

secondleft = 0
secondright = 0
thirdleft = 0
thirdright = 0

! If the second pair is behind the counter-second pair,
! further reasoning with it is pointless. (If the counter-
! second pair is behind the second pair, still the counter-
! second pair must ALSO be behind the third pair later on,
! or else the second atom is "eliminated" before it can
! contribute to a conclusion.)

! Assume the second pair is vic:
if ((X > 0) .AND. (Y > 0)) then

! Determine whether the second pair is seen first, or one of its
! three possible contradictions.
  secminloc = gothroughatoms ! We are seeing the second pair RIGHT NOW.

  countersecminloc1 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == Y) .AND. &
&   (arrayofallatoms(:,r) == X)))

  countersecminloc2 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == -X) .AND. &
&   (arrayofallatoms(:,r) == -Y)))

  countersecminloc3 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == -Y) .AND. &
&   (arrayofallatoms(:,r) == -X)))

  ! Determine the "best" countersecminloc.
  countersecminloc = countersecminloc1

  if ((countersecminloc2 /= 0) .AND. &
& (countersecminloc2 < countersecminloc)) then
    countersecminloc = countersecminloc2
  end if

  if ((countersecminloc3 /= 0) .AND. &
& (countersecminloc3 < countersecminloc)) then
    countersecminloc = countersecminloc3
  end if

  if (((secminloc > countersecminloc) .AND. (countersecminloc /= 0)) &
& .OR. &
& ((secminloc == 0) .AND. (countersecminloc /= 0))) then
    cycle ! We cannot make any conclusions with such a second pair.
  end if

else if ((X < 0) .AND. (Y < 0)) then
! So we are dealing with an ana-pair. This is just like above,
! only that another ana-pair would be no contradiction.

  secminloc = gothroughatoms ! We are seeing the second pair RIGHT NOW.

  countersecminloc1 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == -X) .AND. &
&   (arrayofallatoms(:,r) == -Y)))

  countersecminloc2 = &
&   minloc(arrayofallatoms(:,r), DIM=1, MASK= &
&   ((arrayofallatoms(:,l) == -Y) .AND. &
&   (arrayofallatoms(:,r) == -X)))

  ! Determine the "best" countersecminloc.
  countersecminloc = countersecminloc1

  if ((countersecminloc2 /= 0) .AND. &
& (countersecminloc2 < countersecminloc)) then
    countersecminloc = countersecminloc2
  end if

  if (((secminloc > countersecminloc) .AND. (countersecminloc /= 0)) &
& .OR. &
& ((secminloc == 0) .AND. (countersecminloc /= 0))) then
    cycle ! We cannot make any conclusions with such a second pair.
  end if

else    ! "Else" would be some absurdity like X>0 & Y<0,
  cycle ! and we do not reason with absurdities.
end if

! Note that from now on, countersecminloc will measure the third pair:
if (countersecminloc == 0) then ! the third pair must be encountered
  countersecminloc = sizeofall  ! BEFORE countersecminloc.
end if

! In each case, either we have:
! either
! - two confirmations, two contradictions when the conclusion is ana;
! or
! - one confirmation, three contradictions when the conclusion is vic;
! so in reality this looks more complicated than it actually is.

! Now come sixteen possible cases for triangulation.
! Both the first pair and the second pair are vic.
if (((A > 0) .AND. (B > 0)) .AND. ((X > 0) .AND. (Y > 0))) then

  if (A == X) then

    confA1 = -B
    confB1 = -Y

    confA2 = -Y
    confB2 = -B

    antiA1 = B
    antiB1 = Y

    antiA2 = Y
    antiB2 = B

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    confminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB2)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    if ((confminloc2 /= 0) .AND. &
&   (confminloc2 < confminloc)) then
      confminloc = confminloc2
      thirdleft = confA2
      thirdright = confB2
    end if

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (B == Y) then

    confA1 = -A
    confB1 = -X

    confA2 = -X
    confB2 = -A

    antiA1 = A
    antiB1 = X

    antiA2 = X
    antiB2 = A

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    confminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB2)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    if ((confminloc2 /= 0) .AND. &
&   (confminloc2 < confminloc)) then
      confminloc = confminloc2
      thirdleft = confA2
      thirdright = confB2
    end if

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (A == Y) then

    confA1 = X
    confB1 = B

    antiA1 = B
    antiB1 = X

    antiA2 = -X
    antiB2 = -B

    antiA3 = -B
    antiB3 = -X

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (B == X) then

    confA1 = A
    confB1 = Y

    antiA1 = Y
    antiB1 = A

    antiA2 = -A
    antiB2 = -Y

    antiA3 = -Y
    antiB3 = -A

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else
    cycle ! There is no further evaluation in this loop iteration.
  end if

! Both the first pair and the second pair are ana.
else if (((A < 0) .AND. (B < 0)) .AND. ((X < 0) .AND. (Y < 0))) then

  if (A == X) then

    confA1 = B
    confB1 = Y

    confA2 = Y
    confB2 = B

    antiA1 = -B
    antiB1 = -Y

    antiA2 = -Y
    antiB2 = -B

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    confminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB2)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    if ((confminloc2 /= 0) .AND. &
&   (confminloc2 < confminloc)) then
      confminloc = confminloc2
      thirdleft = confA2
      thirdright = confB2
    end if

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (B == Y) then

    confA1 = A
    confB1 = X

    confA2 = X
    confB2 = A

    antiA1 = -A
    antiB1 = -X

    antiA2 = -X
    antiB2 = -A

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    confminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB2)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    if ((confminloc2 /= 0) .AND. &
&   (confminloc2 < confminloc)) then
      confminloc = confminloc2
      thirdleft = confA2
      thirdright = confB2
    end if

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (A == Y) then

    confA1 = X
    confB1 = B

    confA2 = B
    confB2 = X

    antiA1 = -X
    antiB1 = -B

    antiA2 = -B
    antiB2 = -X

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    confminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB2)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    if ((confminloc2 /= 0) .AND. &
&   (confminloc2 < confminloc)) then
      confminloc = confminloc2
      thirdleft = confA2
      thirdright = confB2
    end if

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (B == X) then

    confA1 = A
    confB1 = Y

    confA2 = Y
    confB2 = A

    antiA1 = -A
    antiB1 = -Y

    antiA2 = -Y
    antiB2 = -A

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    confminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB2)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    if ((confminloc2 /= 0) .AND. &
&   (confminloc2 < confminloc)) then
      confminloc = confminloc2
      thirdleft = confA2
      thirdright = confB2
    end if

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else
    cycle
  end if


! The first pair is vic, but the second pair is ana.
else if (((A > 0) .AND. (B > 0)) .AND. ((X < 0) .AND. (Y < 0))) then

  if (-A == X) then

    confA1 = -Y
    confB1 = B

    antiA1 = B
    antiB1 = -Y

    antiA2 = Y
    antiB2 = -B

    antiA3 = -B
    antiB3 = Y

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (-B == Y) then

    confA1 = A
    confB1 = -X

    antiA1 = -X
    antiB1 = A

    antiA2 = -A
    antiB2 = X

    antiA3 = X
    antiB3 = -A

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (-A == Y) then

    confA1 = -X
    confB1 = B

    antiA1 = B
    antiB1 = -X

    antiA2 = X
    antiB2 = -B

    antiA3 = -B
    antiB3 = X

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (-B == X) then

    confA1 = A
    confB1 = -Y

    antiA1 = -Y
    antiB1 = A

    antiA2 = -A
    antiB2 = Y

    antiA3 = Y
    antiB3 = -A

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else
    cycle
  end if


! The first pair is ana, but the second pair is vic.
else if (((A < 0) .AND. (B < 0)) .AND. ((X > 0) .AND. (Y > 0))) then

  if (A == -X) then

    confA1 = -B
    confB1 = Y

    antiA1 = Y
    antiB1 = -B

    antiA2 = B
    antiB2 = -Y

    antiA3 = -Y
    antiB3 = B

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (B == -Y) then

    confA1 = X
    confB1 = -A

    antiA1 = -A
    antiB1 = X

    antiA2 = -X
    antiB2 = A

    antiA3 = A
    antiB3 = -X

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (A == -Y) then

    confA1 = X
    confB1 = -B

    antiA1 = -B
    antiB1 = X

    antiA2 = -X
    antiB2 = B

    antiA3 = B
    antiB3 = -X

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else if (B == -X) then

    confA1 = -A
    confB1 = Y

    antiA1 = Y
    antiB1 = -A

    antiA2 = A
    antiB2 = -Y

    antiA3 = -Y
    antiB3 = A

    ! Establish whether the third atom has been concluded
    ! BEFORE its contradiction.
    confminloc = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == confA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == confB1)))

    secondleft = X
    secondright = Y

    confminloc = confminloc1
    thirdleft = confA1
    thirdright = confB1

    antiminloc1 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA1) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB1)))

    antiminloc2 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA2) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB2)))

    antiminloc3 = &
&   minloc(arrayofallatoms(:searchlimit,l), DIM=1, MASK= &
&   ((arrayofallatoms(:searchlimit,l) == antiA3) .AND. &
&   (arrayofallatoms(:searchlimit,r) == antiB3)))

    antiminloc = antiminloc1

    if ((antiminloc2 /= 0) .AND. &
&   (antiminloc2 < antiminloc)) then
      antiminloc = antiminloc2
    end if

    if ((antiminloc3 /= 0) .AND. &
&   (antiminloc3 < antiminloc)) then
      antiminloc = antiminloc3
    end if

    ! No further triangulation can be undertaken if the counter-pair
    ! to the third pair is found before the third pair itself. Then
    ! you cannot yet return - but you can cycle. A position of "0"
    ! means "not found"
    if (((confminloc > antiminloc) .AND. (antiminloc /= 0)) &
&   .OR. &
&   ((confminloc == 0) .AND. (antiminloc /= 0)) &
&   .OR. &
&   (confminloc > countersecminloc)) then
      cycle
    else
      triangulate(recordpair, 1) = secondleft
      triangulate(recordpair, 2) = secondright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
      triangulate(recordpair, 1) = thirdleft
      triangulate(recordpair, 2) = thirdright
      recordpair = recordpair + 1
      if (recordpair > globallimit) then
        return
      end if
    end if

  else
    cycle
  end if

end if

end do

return

! End of huge bracing loop for analysing the list of all atoms.
end function triangulate


! pure integer
function &
& filteratoms(arrayofallatoms, tripairs)
! This function merges the tripairs into the array of all atoms,
! however, it preserves the atom numbers so they can be recycled.
implicit none
integer, parameter :: sizeofall = 100000
integer, parameter :: l = 1
integer, parameter :: r = 2
integer :: filteratoms(sizeofall,3)
integer, intent(in) :: arrayofallatoms(sizeofall,3)
integer, intent(in) :: tripairs(sizeofall,2)
integer :: positionseen !
integer :: looppairs !
integer :: maxloop

filteratoms(:,:) = arrayofallatoms(:,:)

! do looppairs = 1, 10! max of tripairs
! It should rather be a countdown, the most
! significant conclusion having the "last word.
do looppairs = sizeofall, 1, -1 ! max of tripairs
! Merge the tripairs from back to front, and skip any tripairs that
! are just zeroes - there is nothing to consider with them.

if ((tripairs(looppairs, l) == 0) .AND. &
&   (tripairs(looppairs, r) == 0)) then
!  exit ! Nothing to implant in that case.
! For a countdown, this is replaced by "cycle":
  cycle ! Nothing to implant in that case.
end if

  ! Test whether the next tripair is actually known.
  positionseen = minloc(arrayofallatoms(:,l), DIM=1, MASK= &
& ((filteratoms(:,l) == tripairs(looppairs,l)) .AND. &
& (filteratoms(:,r) == tripairs(looppairs,r))))

  if ((positionseen == 0) .AND. &
&   (tripairs(looppairs,l) < 0) .AND. (tripairs(looppairs,l) < 0)) then

    ! Give an analogy pair a second chance by looking for its reverse:
    positionseen = minloc(arrayofallatoms(:,l), DIM=1, MASK= &
&   ((filteratoms(:,l) == tripairs(looppairs,r)) .AND. &
&   (filteratoms(:,r) == tripairs(looppairs,l))))

  end if

  if (positionseen /= 0) then
  ! If the next tripair is actually known, just cshift it around
  ! into place.
    filteratoms(:positionseen,:) = &
&   cshift(filteratoms(1:positionseen,:), DIM=1, SHIFT=-1)
  else
  ! If it is unknown, it must be created - forget an atom and
  ! re-use the atom number, only overwriting the sub-atoms.
    filteratoms(:,:) = forget(filteratoms(:,:))
    filteratoms(1,l) = tripairs(looppairs,l)
    filteratoms(1,r) = tripairs(looppairs,r)
  end if

end do

return

end function filteratoms


! pure integer
function &
& multiconc(arrayofallatoms, tripairs)
! The purpose of this function is to continue triangulation on the
! sides of triangulation that have been already found. This lets the
! count of conclusions rise very highly very rapidly - this is what I
! call the "snowflake mechanism". It makes the system more (too?)
! "fanciful".
implicit none
integer, parameter :: sizeofall = 100000
integer, parameter :: l = 1
integer, parameter :: r = 2
integer :: multiconc(sizeofall,2) ! where the multiple conclusions
                                  ! shall be stored
integer :: limitmulti ! cannot be longer than the list of all atoms
integer, intent(in) :: tripairs(sizeofall,2)
integer, intent(in) :: arrayofallatoms(sizeofall,3)
integer :: polyheur
integer :: intermedresult(sizeofall,2)
integer :: firstslot
integer :: interslot

! In the beginning, assume there are no extra conclusions:
multiconc(:,:) = 0
multiconc(:,:) = tripairs(:,:) ! Assuming they are the same shape.

! Which is the "first empty slot", where further triangulation results
! can be attached to? I.e. where are the conclusions only "0" rather
! than actual atoms?
firstslot = minloc(tripairs(:,l), DIM=1, MASK= &
& ((tripairs(:,l) == 0) .AND. (tripairs(:,r) == 0)))

if ((firstslot == 0) .OR. (firstslot == 1)) then
  return ! No further conclusions can be learned:
end if ! either none or all are known already.

! At the same time, that is as far as learning can be attempted.
limitmulti = firstslot - 1

do polyheur = 1, limitmulti ! This is NOT a concurrent loop...
! Basically, it could be made concurrent, but should be lazy, too,
! and I do not well see how to implement this otherwise.
! If I were to make it concurrent, the point would be to do
! the length computation only at the end, when I have all intermediate
! results.

  ! Get further conclusions of the current triangulation pair
  ! (polyheur).
  intermedresult(:,:) = &
  & triangulate(arrayofallatoms(:,:), tripairs(polyheur,:))

  ! Merge in the found further conclusions and get rid of eventual
  ! doubled conclusions. This is the only time I use elimdoubles.
  intermedresult(:,:) = &
  & elimdoubles(intermedresult(:,:), tripairs(:,:))

  ! How many extra conclusions have been found for this triangle pair?
  interslot = minloc(intermedresult(:,l), DIM=1, MASK= &
& ((intermedresult(:,l) == 0) .AND. (intermedresult(:,r) == 0)))

  if (interslot == 1) then
    cycle ! no conclusions found - give the next pair a chance.
  end if

  if (interslot == 0) then
    interslot = sizeofall + 1 ! max length of intermedresult + 1
  end if

  if (firstslot + interslot - 1 < sizeofall) then
                                  ! the max length of multiconc
  ! Then that means that multiconc can absorb all the further
  ! conclusions by appending them entirely.

    multiconc(firstslot:(firstslot + interslot - 1),:) = &
&   intermedresult(1:(interslot - 1),:)

    firstslot = firstslot + interslot
    ! progress with firstslot for the next match

  else
  ! Else interslot must be cut down appropriately to fit in as much
  ! of the new conclusions as possible (but not more).

    interslot = 10 - firstslot + 1 !120= max length of multiconc

    multiconc(firstslot:(firstslot + interslot - 1),:) = &
&   intermedresult(1:(interslot - 1),:)

    return ! further reasoning is pointless

  end if

end do

return

end function multiconc


! pure integer
function &
& resultplan(arrayofallatoms, lastia)
! The purpose for this function is to find the sub-atom that
! is the continuation of the most recent input atom. This
! sub-atom is then to be decomposed into its most elementary
! ingredients which form the answer. - If no sub-atom can be
! found immediately, the most recent input atom has to be split
! and its right half is the new "most recent input atom". If no
! more split is possible, then there is no known answer to the input.
implicit none
integer, parameter :: sizeofall = 100000
integer, parameter :: l = 1
integer, parameter :: r = 2
integer, parameter :: sym = 3
integer, parameter :: iosize = 127
! implement an atom number as a third part of the arrayofallatoms!
integer, parameter :: maxplanlength = iosize
integer :: resultplan(iosize)
integer, intent(in) :: arrayofallatoms(sizeofall,3)
integer, intent(in) :: lastia
integer :: lastinputatom
integer :: highplan
integer :: planarray(iosize) ! maximum as big as resultplan.
integer :: planposition
integer :: decomposehere
integer :: highplanplace
integer :: splitinputplace

lastinputatom = lastia ! lastia = "last input atom", but read-only
resultplan(:) = 0 ! Important - else the plan contains RAM artefacts.

! Search for a neighbour to the last input atom and if none is known,
! split the last input atom and try find the neighbour of that.
do

! print *, "LAST INPUT ATOM =", lastinputatom
  if (lastinputatom == 0) then 
    return ! There is no plan for "no input".
  else
    highplanplace = minloc(arrayofallatoms(:,l), DIM=1, MASK= &
&   (arrayofallatoms(:,l) == lastinputatom))

    if (highplanplace == 0) then

      splitinputplace = minloc(arrayofallatoms(:,sym), DIM=1, MASK= &
&     (arrayofallatoms(:,sym) == lastinputatom))

      ! Without this test, it was just repeating everything
      ! except the first atom.
      if (splitinputplace == 0) then
        return ! No plan. 
      end if

      lastinputatom = arrayofallatoms(splitinputplace,r)
    else
      highplan = arrayofallatoms(highplanplace,r)
      exit ! You GOT a (high) plan, continue now to plan decomposition.
    end if
  end if
end do

planarray(:) = 0
planposition = 1
planarray(planposition) = highplan

do ! Here you "unfold" the plan (shelving off the end eventual
   ! elements that were not yet unfolded - if the space is needed).

    ! Whose super-atom is the first atom of the plan array?
    decomposehere = minloc(arrayofallatoms(:,sym), DIM=1, MASK= &
&   (arrayofallatoms(:,sym) == planarray(1)))

  ! If there is no superatom match, it means the atom is elementary
  ! and can therefore be included in the resultplan.
  if (decomposehere == 0) then
    ! So far, not testing the limit for elementary atoms.

    ! The resulting plan gets one more element.
    resultplan(planposition) = planarray(1)
    if (planposition == maxplanlength) then
      return
    end if
    planposition = planposition + 1

    if (planposition > iosize) then
      return ! The plan cannot sensibly become bigger than this.
    end if

    ! And of course, advance the next element for analysis.
    planarray(:) = eoshift(planarray(:), SHIFT=1, BOUNDARY=0)
    if (planarray(1) == 0) then
      return ! There is nothing further to decompose.
    end if
  else
    ! The atom was not elementary, so re-attach its sub-atoms
    ! on the front of plan array, and try to next analyse the
    ! "new" first element of the plan array:
    planarray(:) = eoshift(planarray(:), SHIFT=-1, BOUNDARY=0)
    planarray(1) = arrayofallatoms(decomposehere, l)
    planarray(2) = arrayofallatoms(decomposehere, r)
  end if
end do

return

end function resultplan


! The below function became a bit messy.
! Which pair will be selected will be determined by tripairs:
! either the right-most combination wins, if all are equal, or
! that combination that has the most conclusions.
! pure integer
function &
& hierarchise(inputlist, selectedpair, highatom)
! This function actually should do something very simple:
! it is replacing a "selected pair" in the input list with some
! super-atom ("highatom"), whose sub-atoms the "selected pair" forms.
! This is a batch replacement, i.e. not only one single pair is
! replaced, but each occurrence of the pair in the input list.
implicit none
integer, parameter :: iosize = 127
integer :: hierarchise(iosize)
integer, intent(in) :: inputlist(iosize)
integer, intent(in) :: selectedpair(2)
integer, intent(in) :: highatom
integer :: counthier
integer :: prevatom
integer :: nextatom
integer :: resultcounter
logical :: setcycle

hierarchise(:) = 0
resultcounter = 1
setcycle = .FALSE.
! iosize is being transgressed in order to avoid having to handle
! "margin issues" with regard to prevatom and nextatom.
do counthier = 1, iosize + 1 ! iosize + 1 works

  ! The resultcounter measures the answer, and
  ! so transgressing the size of input is certainly disallowed.
  if (resultcounter > iosize) then
    exit
  end if

  ! setcycle serves to skip an atom just after having recognised
  ! the selected pair in the input list. This avoids double analysis
  ! of the second atom of the selected pair.
  if (setcycle .EQV. .TRUE.) then
    setcycle = .FALSE.
    cycle
  end if

  ! As I am running above iosize, if counthier becomes too high, then
  ! prevatom and nextatom cannot have array values and are simply 0.
  if (counthier <= iosize) then
    prevatom = inputlist(counthier)
  else
    prevatom = 0
  end if

  if (counthier <= (iosize - 1)) then
    nextatom = inputlist(counthier + 1)
  else
    nextatom = 0
  end if

  ! Now either catch the pair you are looking for, or simply record one
  ! more atom in the resulting hierarchise-list.
  if ((prevatom == selectedpair(1)) .AND. &
&     (nextatom == selectedpair(2))) then
    hierarchise(resultcounter) = highatom
    resultcounter = resultcounter + 1
    setcycle = .TRUE.
  else 
    hierarchise(resultcounter) = prevatom
    resultcounter = resultcounter + 1
  end if

end do

return

end function hierarchise

end program pter


! This subroutine handles reading in the string
! into a numeric input array.
subroutine inreader(lengthofinput,parsedarrayposition, &
& inputstring,wordlist,parsedarray)
implicit none

integer, parameter :: maxinlen = 2048
character(LEN=maxinlen) :: inputstring
integer, parameter :: wordlistlength = 8190
integer, parameter :: unknownword = wordlistlength ! Cannot be 0,
! as that means end of string.
integer, parameter :: wordsize = 32
character(LEN=wordsize) :: wordlist(wordlistlength)
integer, parameter :: parsedarraylength = 2048 ! how many words can be
! parsed at once.
integer :: parsedarray(parsedarraylength)
integer :: parsedarrayposition

logical :: wordterminated ! has a word been ended
logical :: nowordpreceding ! was no word seen before the current sign
logical :: onlyseparator ! is the sign not a word,
                         ! just a sentence separator
integer :: delimitersign ! if so, which sentence separator was it
integer, parameter :: notknownlength = 7 ! length of the word "UNKNOWN"
character(LEN=wordsize), parameter :: notknownword = "UNKNOWN"
character(LEN=wordsize) :: inputword
integer :: inputwordcounter
integer :: inputreader
integer :: lengthofinput
character :: harvestedchar
logical :: wordwasfound
integer :: findthatword
integer, parameter :: abovedelimitersigns = 49

! BESIDES THE LOADED WORDS, THERE ARE ALSO THE BASIC SIGNS WHICH ARE
! NOT WRITTEN TO FILE:

wordlist(1) = " "
wordlist(2) = "!"
wordlist(3) = char(34) ! DOUBLEQUOTE
wordlist(4) = "#"
wordlist(5) = "$"
wordlist(6) = "%"
wordlist(7) = "&"
wordlist(8) = "'"
wordlist(9) = "("
wordlist(10) = ")"
wordlist(11) = "*"
wordlist(12) = "+"
wordlist(13) = ","
wordlist(14) = "-"
wordlist(15) = "."
wordlist(16) = "/"
wordlist(17) = "1"
wordlist(18) = "2"
wordlist(19) = "3"
wordlist(20) = "4"
wordlist(21) = "5"
wordlist(22) = "6"
wordlist(23) = "7"
wordlist(24) = "8"
wordlist(25) = "9"
wordlist(26) = "0"
wordlist(27) = "	" ! Tabulator
wordlist(28) = ":"
wordlist(29) = ";"
wordlist(30) = "<"
wordlist(31) = "="
wordlist(32) = ">"
wordlist(33) = "?"
wordlist(34) = "@"
wordlist(35) = "´"
wordlist(36) = "`"
wordlist(37) = "{"
wordlist(38) = "}"
wordlist(39) = "|"
wordlist(40) = "~"
wordlist(41) = "["
wordlist(42) = "]"
wordlist(43) = "^"
wordlist(44) = "°"
wordlist(45) = "_"
wordlist(46) = "§"
wordlist(47) = char(10)
wordlist(48) = char(13)

nowordpreceding = .TRUE.
wordterminated = .TRUE.
onlyseparator = .TRUE.
inputword = ""
inputwordcounter = 1
parsedarray(:) = 0
do inputreader = 1, lengthofinput

  nowordpreceding = wordterminated

  wordterminated = .FALSE.
  harvestedchar = inputstring(inputreader:inputreader)

  delimitersign = 0
  if (harvestedchar == " ") then
    wordterminated = .TRUE.
!    delimitersign = 1 ! LET US ASSUME THAT SPACES SHALL NOT BE COUNTED
!    AS DELIMITERS TO BE WORTHY OF RECORDING. This is a design choice,
!    not an objectively justifiable necessity.
  else if (harvestedchar == "!") then
    wordterminated = .TRUE.
    delimitersign = 2
  else if (harvestedchar == char(34)) then ! DOUBLEQUOTE
    wordterminated = .TRUE.
    delimitersign = 3
  else if (harvestedchar == "#") then
    wordterminated = .TRUE.
    delimitersign = 4
  else if (harvestedchar == "$") then
    wordterminated = .TRUE.
    delimitersign = 5
  else if (harvestedchar == "%") then
    wordterminated = .TRUE.
    delimitersign = 6
  else if (harvestedchar == "&") then
    wordterminated = .TRUE.
    delimitersign = 7
  else if (harvestedchar == "'") then
    wordterminated = .TRUE.
    delimitersign = 8
  else if (harvestedchar == "(") then
    wordterminated = .TRUE.
    delimitersign = 9
  else if (harvestedchar == ")") then
    wordterminated = .TRUE.
    delimitersign = 10
  else if (harvestedchar == "*") then
    wordterminated = .TRUE.
    delimitersign = 11
  else if (harvestedchar == "+") then
    wordterminated = .TRUE.
    delimitersign = 12
  else if (harvestedchar == ",") then
    wordterminated = .TRUE.
    delimitersign = 13
  else if (harvestedchar == "-") then
    wordterminated = .TRUE.
    delimitersign = 14
  else if (harvestedchar == ".") then
    wordterminated = .TRUE.
    delimitersign = 15
  else if (harvestedchar == "/") then
    wordterminated = .TRUE.
    delimitersign = 16
  else if (harvestedchar == "1") then
    wordterminated = .TRUE.
    delimitersign = 17
  else if (harvestedchar == "2") then
    wordterminated = .TRUE.
    delimitersign = 18
  else if (harvestedchar == "3") then
    wordterminated = .TRUE.
    delimitersign = 19
  else if (harvestedchar == "4") then
    wordterminated = .TRUE.
    delimitersign = 20
  else if (harvestedchar == "5") then
    wordterminated = .TRUE.
    delimitersign = 21
  else if (harvestedchar == "6") then
    wordterminated = .TRUE.
    delimitersign = 22
  else if (harvestedchar == "7") then
    wordterminated = .TRUE.
    delimitersign = 23
  else if (harvestedchar == "8") then
    wordterminated = .TRUE.
    delimitersign = 24
  else if (harvestedchar == "9") then
    wordterminated = .TRUE.
    delimitersign = 25
  else if (harvestedchar == "0") then
    wordterminated = .TRUE.
    delimitersign = 26
  else if (harvestedchar == "	") then ! I HAVE SPACE ALREADY!
    wordterminated = .TRUE.
    delimitersign = 27
  else if (harvestedchar == ":") then
    wordterminated = .TRUE.
    delimitersign = 28
  else if (harvestedchar == ";") then
    wordterminated = .TRUE.
    delimitersign = 29
  else if (harvestedchar == "<") then
    wordterminated = .TRUE.
    delimitersign = 30
  else if (harvestedchar == "=") then
    wordterminated = .TRUE.
    delimitersign = 31
  else if (harvestedchar == ">") then
    wordterminated = .TRUE.
    delimitersign = 32
  else if (harvestedchar == "?") then
    wordterminated = .TRUE.
    delimitersign = 33
  else if (harvestedchar == "@") then
    wordterminated = .TRUE.
    delimitersign = 34
  else if (harvestedchar == "´") then
    wordterminated = .TRUE.
    delimitersign = 35
  else if (harvestedchar == "`") then
    wordterminated = .TRUE.
    delimitersign = 36
  else if (harvestedchar == "{") then
    wordterminated = .TRUE.
    delimitersign = 37
  else if (harvestedchar == "}") then
    wordterminated = .TRUE.
    delimitersign = 38
  else if (harvestedchar == "|") then
    wordterminated = .TRUE.
    delimitersign = 39
  else if (harvestedchar == "~") then
    wordterminated = .TRUE.
    delimitersign = 40
  else if (harvestedchar == "[") then
    wordterminated = .TRUE.
    delimitersign = 41
  else if (harvestedchar == "]") then
    wordterminated = .TRUE.
    delimitersign = 42
  else if (harvestedchar == "^") then
    wordterminated = .TRUE.
    delimitersign = 43
  else if (harvestedchar == "°") then
    wordterminated = .TRUE.
    delimitersign = 44
  else if (harvestedchar == "_") then
    wordterminated = .TRUE.
    delimitersign = 45
  else if (harvestedchar == "§") then
    wordterminated = .TRUE.
    delimitersign = 46
  else if (harvestedchar == char(10)) then ! THAT IS QUESTIONABLE.
  ! - THEN THE TRAINING TEXT SHOULD NOT CONTAIN THEM!
    wordterminated = .TRUE.
  !  delimitersign = 47  ! I AM HEREWITH ELIMINATING CHAR-10!
  else if (harvestedchar == char(13)) then ! IF CR&LF ARE USED TO
  ! FORMAT THE TEXT, THEY SHOULD RATHER BE USED EXACTLY LIKE SPACE.
    wordterminated = .TRUE.
  !  delimitersign = 48  ! I AM HEREWITH ELIMINATING CHAR-13!
  ! AFTER ELIMINATING CHAR-10 & CHAR-13 (I.E. TREATING THEM EXACTLY
  ! EXACTLY "SPACE"), THE INPUT CAN BE ON SEVERAL LINES.
  end if

  ! NEW ELEMENT:
  ! Optional: a "capitaliser" function.
  if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "a")) then
    harvestedchar = "A"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "b")) then
    harvestedchar = "B"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "c")) then
    harvestedchar = "C"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "d")) then
    harvestedchar = "D"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "e")) then
    harvestedchar = "E"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "f")) then
    harvestedchar = "F"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "g")) then
    harvestedchar = "G"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "h")) then
    harvestedchar = "H"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "i")) then
    harvestedchar = "I"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "j")) then
    harvestedchar = "J"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "k")) then
    harvestedchar = "K"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "l")) then
    harvestedchar = "L"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "m")) then
    harvestedchar = "M"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "n")) then
    harvestedchar = "N"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "o")) then
    harvestedchar = "O"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "p")) then
    harvestedchar = "P"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "q")) then
    harvestedchar = "Q"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "r")) then
    harvestedchar = "R"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "s")) then
    harvestedchar = "S"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "t")) then
    harvestedchar = "T"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "u")) then
    harvestedchar = "U"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "v")) then
    harvestedchar = "V"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "w")) then
    harvestedchar = "W"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "x")) then
    harvestedchar = "X"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "y")) then
    harvestedchar = "Y"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "z")) then
    harvestedchar = "Z"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "-")) then
    harvestedchar = "-"
  else if ((wordterminated .EQV. .FALSE.) .AND. &
&   (harvestedchar == "ß")) then
    harvestedchar = "S"
  else
    harvestedchar = harvestedchar ! "X" -- THAT "X" WAS CAUSING TROUBLE
  end if
  ! END OF NEW ELEMENT

  ! You have only a separator sign if the "word" is terminated, but no
  ! word has been seen before it; such may be observed in "..." or "?!"
  onlyseparator = wordterminated .AND. nowordpreceding

  if (wordterminated .EQV. .TRUE.) then
    if (onlyseparator .EQV. .TRUE.) then
      if (delimitersign /= 0) then
        parsedarray(parsedarrayposition) = delimitersign
        parsedarrayposition = parsedarrayposition + 1
      else
!       print *, "ignore whitespace", &
! &     parsedarray(1:parsedarrayposition - 1)
      end if
    else
      if (delimitersign /= 0) then
        wordwasfound = .false.
        do findthatword = abovedelimitersigns, wordlistlength
        ! There is no point in   searching through the word delimiters.
          if (wordlist(findthatword) == "") then
            ! It means we did not find the atom, but on the other hand,
            ! there is still space for new elementary atoms.
            ! Hence, we shall create a new one.
            wordlist(findthatword) = inputword
            wordwasfound = .true.
            ! And which word it was is saved in
            ! findthatword's looping stage.
            exit
          else if (wordlist(findthatword) == inputword) then
            ! It means we DID find the atom and can thus terminate.
            wordwasfound = .true.
            ! And which word it was is saved in
            ! findthatword's looping stage.
            exit
          end if
        end do

        if (wordwasfound .EQV. .TRUE.) then
          parsedarray(parsedarrayposition) = findthatword
          parsedarrayposition = parsedarrayposition + 1
        else
          parsedarray(parsedarrayposition) = unknownword ! means the
          ! word was UNKNOWN and UNCREATABLE
          parsedarrayposition = parsedarrayposition + 1
        end if
        parsedarray(parsedarrayposition) = delimitersign
        parsedarrayposition = parsedarrayposition + 1
      else
        wordwasfound = .false.
        ! You only search till wordlistlength - 1, because
        ! wordlist(wordlistlength) is the UNKNOWN atom.
        do findthatword = abovedelimitersigns, wordlistlength - 1
        ! There is no point in searching through the word delimiters.
          if (wordlist(findthatword) == "") then
            ! It means we did not find the atom, but on the other hand,
            ! there is still space for new elementary atoms.
            ! Hence, we shall create a new one.
            wordlist(findthatword) = inputword
            wordwasfound = .true.
            ! And which word it was is saved in
            ! findthatword's looping stage.
            exit
          else if (wordlist(findthatword) == inputword) then
            ! It means we DID find the atom and can thus terminate.
            wordwasfound = .true.
            ! And which word it was is saved in
            ! findthatword's looping stage.
            exit
          end if
        end do

        if (wordwasfound .EQV. .TRUE.) then
          parsedarray(parsedarrayposition) = findthatword
          parsedarrayposition = parsedarrayposition + 1
        else
          parsedarray(parsedarrayposition) = unknownword ! means the
          ! word was UNKNOWN and UNCREATABLE
          parsedarrayposition = parsedarrayposition + 1
        end if
      end if

      inputword = ""
      inputwordcounter = 1
    end if
  else
    if (inputwordcounter < wordsize) then
      inputword(inputwordcounter:inputwordcounter) = harvestedchar
      inputwordcounter = inputwordcounter + 1
    end if
  end if

end do

end subroutine inreader



! This subroutine turns the numeric answer into a string.
subroutine outwriter(nextwordposition,outputstring,wordlist,planarray)
implicit none

integer, parameter :: maxinlen = 2048
integer, parameter :: maxoutputlen = maxinlen
character(LEN=maxoutputlen) :: outputstring
integer, parameter :: parsedarraylength = 2048
integer, parameter :: planarraylength = parsedarraylength ! how many
! words can be parsed at once.
integer :: planarray(planarraylength)
integer :: nextwordposition
integer :: findwordlength
integer :: wordlength
logical :: timetoexit

integer :: outputreader
integer, parameter :: wordlistlength = 8190
integer, parameter :: wordsize = 32
character(LEN=wordsize) :: wordlist(wordlistlength)
character(LEN=wordsize) :: outputword
integer, parameter :: abovedelimitersigns = 49

outputstring = ""
nextwordposition = 1

do outputreader = 1, planarraylength
  timetoexit = .FALSE.

  if (planarray(outputreader) == 0) then
    exit
  else ! This will work even for the unknown word, as it
       ! will be matched by wordlist(wordlistlength)

    ! Find out how long the next word is.
    wordlength = wordsize
    outputword = wordlist(planarray(outputreader))
    do findwordlength = 1, wordsize
      if (outputword(findwordlength:findwordlength) == " ") then
        wordlength = findwordlength - 1
        exit
      end if
    end do

    ! Attach that word to the output string
    if (maxoutputlen - (nextwordposition + wordlength + 1) &
&       >= wordlength) then
       ! MTIO = May Turn It Off - if you do not want sentence sign
       ! special treatment
      if (planarray(outputreader) >= abovedelimitersigns) then ! MTIO
        outputstring(nextwordposition:nextwordposition &
&                    + wordlength) = &
&       outputword(1:wordlength)
        nextwordposition = nextwordposition + wordlength + 1
      else ! MTIO
        if (nextwordposition > 1) then ! MTIO
          nextwordposition = nextwordposition - 1 ! MTIO
        end if ! MTIO
        outputstring(nextwordposition:nextwordposition &
&                    + wordlength) = &
&       outputword(1:wordlength) ! MTIO
        nextwordposition = nextwordposition + wordlength + 1 ! MTIO
      end if ! MTIO

    else
      timetoexit = .TRUE.
    end if
  end if

  if (timetoexit .EQV. .TRUE.) then
    exit
  end if
end do

end subroutine outwriter


