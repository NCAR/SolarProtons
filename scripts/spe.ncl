; Processing of GOES Solar Proton data
;
; This uses a fortran program that was initially supplied by Charley
; Jackman, but has been adapted by me to calculate ion production
; rates from GOES solar proton data.
;
; See http://solarisheppa.geomar.de/solarprotonfluxes for some additional
; descriptions.
;
; This data was downloaded by from anonymous ftp at:
;   umbra.nascom.nasa.gov
;   /sdb/goes/particle/
;
; Charles Bardeen
; October 2016

load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclex/gsun/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"


; Generate a file prefix for the specified output format.
undef("spe_prefix4format")
function spe_prefix4format(format)

begin

  ; Generate a prefix for the format.
  prefix = "unknown"
  if (format .eq. 1) then
    prefix = "w4"
  end if
  if (format .eq. 2) then
    prefix = "w6"
  end if
  if (format .eq. 3) then
    prefix = "c6"
  end if

  return(prefix)
end

undef("getdimsize")
function getdimsize(hfile, dimname)

local names, nnames, ndims, ndim, i
begin
  names  = getvardims(hfile)
  nnames = dimsizes(names)

  ndims  = getfiledimsizes(hfile)

  ndim   = -1
  do i = 0, nnames-1
    if (names(i) .eq. dimname) then
      ndim = ndims(i)
      break
    end if
  end do

  return(ndim)
end

undef("getTxtFiles")
procedure getTxtFiles(year, month, day)

begin
  system("wget -P noaa_txt https://umbra.nascom.nasa.gov:/sdb/goes/particle/" + year + month + day + "_Gp_part_5m.txt")
  system("wget -P noaa_txt https://umbra.nascom.nasa.gov:/sdb/goes/particle/" + year + month + day + "_Gs_part_5m.txt")
end


; Read in the GOES solar proton and electron fluxes from NOAA
; ASCII text format and convert to NETCDF.
;
; This is an example header for the text files:
;
;  :Data_list: 20110417_Gp_part_5m.txt
;  :Created: 2011 Apr 18 0011 UTC
;  # Prepared by the U.S. Dept. of Commerce, NOAA, Space Weather Prediction Center
;  # Please send comments and suggestions to SWPC.Webmaster@noaa.gov
;  #
;  # Label: P > 1 = Particles at >1 Mev
;  # Label: P > 5 = Particles at >5 Mev
;  # Label: P >10 = Particles at >10 Mev
;  # Label: P >30 = Particles at >30 Mev
;  # Label: P >50 = Particles at >50 Mev
;  # Label: P>100 = Particles at >100 Mev
;  # Label: E>0.8 = Electrons at >0.8 Mev
;  # Label: E>2.0 = Electrons at >2.0 Mev
;  # Label: E>4.0 = Electrons at >4.0 Mev
;  # Units: Particles = Protons/cm2-s-sr
;  # Units: Electrons = Electrons/cm2-s-sr
;  # Source: GOES-13
;  # Location: W075
;  # Missing data: -1.00e+05
;  #
;  #                      5-minute  GOES-13 Solar Particle and Electron Flux
;  #
;  #                 Modified Seconds
;  # UTC Date  Time   Julian  of the
;  # YR MO DA  HHMM    Day     Day     P > 1     P > 5     P >10     P >30     P >50     P>100     E>0.8     E>2.0     E>4.0
;  #-------------------------------------------------------------------------------------------------------------------------
;
;
; NOTE: This routine is set up to process one day at a time.
undef("spe_noaa2nc")
procedure spe_noaa2nc(isat, osat, year, month, day)

local i, j

begin

  ; Combine fluxes into one variable indexed by time and energy threshold.
  penergy = (/ 1., 5., 10., 30., 50., 100. /)
  penergy@long_name = "Proton energy threshold"
  penergy@units     = "MeV"
  nPenergy = dimsizes(penergy)

  eenergy = (/ 0.8, 2.0, 4.0 /)
  eenergy@long_name = "Electron energy threshold"
  eenergy@units     = "MeV"
  nEenergy = dimsizes(eenergy)


  ; Build the path to the file.
  noaafile = "noaa_txt/" + year + month + day + isat + "_part_5m.txt"
  if (fileexists(noaafile)) then
    print("  " + noaafile)
  else

    ; Older files are missing an '_' in the name so try again.
    noaafile = "noaa_txt/" + year + month + day + isat + "part_5m.txt"
    if (fileexists(noaafile)) then
      print("  " + noaafile)
    else
      print("  skipping ... " + noaafile)
      return
    end if
  end if

  ; Make a list of fields
  field     = (/ "year", "month", "day", "tod", "jday", "jtod", "pflux", "eflux" /)
  nFields   = dimsizes(field)

  type      = (/ "integer", \
                 "integer", \
                 "integer", \
                 "float", \
                 "integer", \
                 "integer", \
                 "float", \
                 "float" \
              /)
  longname  = (/ "UTC Year", \
                 "UTC Month", \
                 "UTC Day", \
                 "UTC Time", \
                 "Julian Day", \
                 "Julian Time", \
                 "Proton Flux", \
                 "Electron Flux" \
              /)
  unit      = (/ "yyyy", \
                 "mm", \
                 "dd", \
                 "hhmm", \
                 "days since 1858-11-17 00:00", \
                 "seconds", \
                 "Protons/cm2-s-sr", \
                 "Electrons/cm2-s-sr", \
                 "Electrons/cm2-s-sr" \
              /)

  offset     = (/ 0, 1, 2, 3, 4, 5, 6, 12 /)
  fillvalue  = -1.00e+5

  ; Skip the file header
  header   = readAsciiHead(noaafile, "#---------")
  hdrSize  = dimsizes(header)

  ; Read the data.
  data = readAsciiTable(noaafile, nFields + nPenergy + nEenergy - 2, "float", hdrSize)

  ; Create a time dimension
  ;
  ; NOTE: This time dimension is based upon the CMIP6 standard time reference.
  hours   = tointeger(floor(data(:, 3) / 100))
  minutes = tointeger(data(:, 3)) - hours * 100

  time           = greg2jul(tointeger(data(:, 0)), tointeger(data(:, 1)), tointeger(data(:, 2)), hours) - greg2jul(1850, 1, 1, 0) + (minutes / 24. / 60.)
  time!0         = "time"
  time&time      = time
  time@long_name = "Time"
  time@units     = "days since 1850-01-01 00:00:00"
  time@calendar  = "gregorian"
  time@bounds    = "time_bnds"
  nTimes         = dimsizes(time)

  ; Create a time bounds variable;
  ;
  ; NOTE: Assume that the data is evenly spaced.
  time_bnds           = new((/ nTimes, 2 /), double)
  time_bnds!0         = "time"
  time_bnds&time      = time
  time_bnds!1         = "nbd"
  time_bnds@long_name = "bounds of time"
  time_bnds@units      = "days since 1850-01-01 00:00:00"
  time_bnds@calendar  = "gregorian"

  dtime               = time(1) - time(0)
  time_bnds(:, 0)     = time(:) - (dtime / 2.)
  time_bnds(:, 1)     = time(:) + (dtime / 2.)

  ; Create the output file.
  system("mkdir -p noaa_5m/" + year)
  ncfile   = "noaa_5m/" + year + "/" + "goes_" + year + month + day + osat + "_part_5m.nc"
  print("    " + ncfile)
  if (isfilepresent(ncfile)) then
    system("rm " + ncfile)
  end if
  ofile    = addfile(ncfile, "c")
  filedimdef(ofile, "time", -1, True)

  ; Write out the time and time bounds.
  ofile->time      = time
  ofile->time_bnds = time_bnds

  ; Write the output fields.
  do i = 0, nFields-1

    ; Create the proper output field type and set the longname and units.
    ; Pack proton and electron fluxes into 1 variable each.
    if (field(i) .eq. "pflux") then
      ovar            = new((/ nTimes, nPenergy /), type(i))
      ovar!0          = "time"
      ovar!1          = "penergy"
      ovar&penergy    = penergy
      ovar@_FillValue = fillvalue

      do j = 0, nPenergy-1
        ovar(:, j) = (/ data(:, offset(i) + j) /)
      end do
    else
      if (field(i) .eq. "eflux") then
        ovar            = new((/ nTimes, nEenergy /), type(i))
        ovar!0          = "time"
        ovar!1          = "eenergy"
        ovar&eenergy    = eenergy
        ovar@_FillValue = fillvalue

        do j = 0, nEenergy-1
          ovar(:, j) = (/ data(:, offset(i) + j) /)
        end do
      else
        if (type(i) .eq. "integer") then
          ovar = (/ tointeger(data(:, offset(i))) /)
        else
          ovar            = (/ data(:, offset(i)) /)
          ovar@_FillValue = fillvalue
        end if

        ovar!0          = "time"
      end if
    end if

    ovar&time         = time
    ovar@long_name    = longname(i)
    ovar@units        = unit(i)

    ofile->$field(i)$ = ovar

    delete(ovar)
  end do
end


; Read in the ion production rates from Charley in
; ASCII text format and convert to NETCDF.
;
; This is an example header for the text files:
;
; Year 1995    Solar Proton-caused Ionization rates #/gm/s - from NOAA GOES
;
;                 Day          Format  1X,1PE11.3,3X,1P6E11.3  for table
;
; Pressure(hPa)       1          2          3          4          5          6
;
; NOTE: This routine is set up to process one year at a time.
undef("spe_ions2nc")
procedure spe_ions2nc(year)

local i, j

begin

  ; Build the path to the file.
  noaafile = "ions_txt/IonPair-gm_Year-" + year + ".dat"
  if (fileexists(noaafile)) then
    print("  " + noaafile)
  else
    print("  skipping ... " + noaafile)
    return
  end if

  ; Read in the data in chuck of 6 days
  nlev   = 58
  nchunk = 61
  itotal = nchunk * (nlev+4) + 1
  if (isleapyear(tointeger(year)))then
    tdays = 366
  else
    tdays = 365
  end if

  ; Create the output file, and set up the time, time_bnds, date and datesec
  ; fields
  system("mkdir -p ions_nc/" + year)
  ncfile   = "ions_nc/" + year + "/IonPair-gm_Year-" + year + ".nc"
  print("    " + ncfile)
  if (fileexists(ncfile)) then
    system("rm " + ncfile)
  end if
  ofile    = addfile(ncfile, "c")
  filedimdef(ofile, "time", -1, True)

  firsttime = greg2jul(tointeger(year), 1, 1, 0)  - greg2jul(1850, 1, 1, 0) + 0.5
  time           = todouble(fspan(firsttime, firsttime+tdays-1, tdays))
  time!0         = "time"
  time&time      = time
  time@long_name = "Time"
  time@units     = "days since 1850-01-01 00:00:00"
  time@calendar  = "gregorian"
  time@bounds    = "time_bnds"
  nTimes         = dimsizes(time)
  ofile->time      = time

  ; Create a time bounds variable;
  ;
  ; NOTE: Assume that the data is evenly spaced.
  time_bnds           = new((/ nTimes, 2 /), double)
  time_bnds!0         = "time"
  time_bnds&time      = time
  time_bnds!1         = "nbd"
  time_bnds@long_name = "bounds of time"
  time_bnds@units      = "days since 1850-01-01 00:00:00"
  time_bnds@calendar  = "gregorian"

  dtime               = time(1) - time(0)
  time_bnds(:, 0)     = time(:) - (dtime / 2.)
  time_bnds(:, 1)     = time(:) + (dtime / 2.)
  ofile->time_bnds = time_bnds

  ; Do date and datesec.
  greg = jul2greg(time(:) + greg2jul(1850, 1, 1, 0))
  date = greg(:,0) * 10000 + greg(:,1) * 100 + greg(:,2)
  date!0 = "time"
  date&time = time
  date@long_name = "date"
  date@units = "YYYYMMDD"
  ofile->date = date

  datesec = new((/ nTimes /), integer)
  datesec!0 = "time"
  datesec&time = time
  datesec(:) = 43200
  datesec@long_name = "current seconds of current date"
  ofile->datesec = datesec

  ; Do the ino production rate.
  prod = new((/ nTimes, nlev /), float)
  prod!0 = "time"
  prod&time = time
  prod!1 = "pressure"
  prod@long_name = "Ion pair production rate"
  prod@units = "/g/sec"

  doy = 0
  do ichunk = 0, nchunk-1
    ihead = (ichunk) * (nlev+4) + 5
    itail = itotal - (ihead + 58)

    ; Read in the pressure levels.
    if ((.not. isleapyear(tointeger(year))) .and. (ichunk .eq. 60)) then
      ndays = 5
    else
      ndays = 6
    end if

    hdrSize = (/ ihead, itail /)
    data = readAsciiTable(noaafile, ndays+1, "float", hdrSize)

    do i = 0, ndays-1
      prod(doy, :) = (/ data(:, i+1) /)
      doy = doy + 1

      ; Write out the data for each day.
;      print("  " + i)
;      print("     " + data(:,0) + "  " + data(:,i+1))
    end do

    if (ichunk .eq. 0) then
      p = new((/ nlev /), float)
      p!0 = "pressure"
      p@long_name = "pressure"
      p@units = "mbar"
      p@positive = "down"
      p = (/ data(:, 0) /)
      p&pressure = p
      prod&pressure = p
    end if

    delete(data)
  end do

  ofile->Prod = prod
end


undef("spe_primary4date")
function spe_primary4date(year, month, day)

local i, sdate, psat, ssat, tdate, sat

begin

  ; The is a lookup table for the primary GOES instrument based upon
  ; XRS measurements and are listed in table 2 at:
  ; https://ngdc.noaa.gov/stp/satellite/goes/doc/GOES_XRS_readme.pdf
  ;
  ; NOTE: Starting on 20100414, the data files from NOAA are already
  ; tagged as primary (Gp) or secondary (Gs), so this table is only
  ; needed for dates before then.
  ;
  ; NOTE: This selection of the primary may be different than the
  ; choices made by Charley when he generated the data set previously;
  ; however, he agreed that this would be a better criteria than what
  ; he used before.
  ;
  ; NOTE: G12 had problems with channels 6 and 7 and never seemed to
  ; generate any proton fluxes. So the XRS table has been update to
  ; remove G12.
  ;
  ; NOTE: Changed it to the following based upon communication from
  ; Charley Jackman:
  ;
  ;  "Several GOES satellites are used for the proton fluxes in years
  ;   1994-2012:
  ;     1) GOES-7 for the period January 1, 1994 through February 28, 1995;
  ;     2) GOES-8 for the period March 1, 1995 through April 8, 2003, and May 10, 2003 to June 18, 2003;
  ;     3) GOES-11 for the period June 19, 2003 to April 13, 2010;
  ;     4) GOES-10 to fill in the gap of missing proton flux data from April 9 through May 9, 2003;
  ;     5) GOES-13 for the period April 14, 2010 to December 31, 2012.   The GOES satellite proton fluxes are fit in three energy intervals 1-10 MeV, 10-50 MeV, and 50-300 MeV with exponential spectral forms.  The energy deposition methodology again is that discussed in Jackman et al. [1980]."
  ;
  ; NOTE: Goes 8 isn't really back until May 14, 2003. Lots of missing data in between May 9 and May 14.
  ; Should use GOES-10 instead.
  sdate = (/ \
    "19940101", \
    "19950301", \
    "20030408", \
    "20030510", \
    "20030619", \
    "20100414" \
  /)
  psat = (/ \
    "G7",  \
    "G8",  \
    "G10",  \
    "G8", \
    "G11", \
    "Gp"  \
  /)

;  print(sdate + "  " + psat)

  ndates = dimsizes(sdate)

  ; Check the date to make sure it is in the table.
  tdate = year + month + day

  if (tdate .lt. sdate(0)) then
    print("  ERROR: Don't know primary satellite for " + tdate)
  else

    sat = ""

    do i = 0, ndates-1
      if (tdate .lt. sdate(i)) then
        break
      end if

      sat = "_" + psat(i)
    end do
  end if

  return(sat)
end

; Average the data to 1 hr.
;
; NOTE: Don't make assumptions about the time spacing in the original
; file, but assume that it is only 1 day per file.
undef("spe_2hourly")
procedure spe_2hourly(sat, prefix, year, month, day)

local i, j

begin

  ; Read in the source data.
  ifile  = "noaa_5m/" + year + "/" + "goes_" + year + month + day + sat + prefix + ".nc"
  if (isfilepresent(ifile)) then
    print("  " + ifile)
  else
    print("  skipping ... " + ifile)
    return
  end if
  x     = addfile(ifile, "r")

  ofile = "noaa_1h/" + year + "/" + "goes_" + year + month + day + sat + "_part_1h.nc"
  print("    " + ofile)
  system("mkdir -p noaa_1h/" + year)
  if (isfilepresent(ofile)) then
    system("rm " + ofile)
  end if
  y     = addfile(ofile, "c")
  filedimdef(y, "time", -1, True)

  ; Create an hourly time for this day.
  time = new((/ 24 /), double)
  time!0 = "time"

  tmp = x->time
  copy_VarAtts(tmp, time)
  time(:) = floor(tmp(0)) + ((ispan(0, 23, 1) + 0.5) / 24.)
  time&time = time

  y->time = time

  ; Create time bounds.
  time_bnds = new((/ 24, 2 /), double)
  time_bnds!0 = "time"
  time_bnds&time = time
  time_bnds!1 = "nbd"

  tmp2 = x->time_bnds
  copy_VarAtts(tmp2, time_bnds)
  time_bnds(:, 0) = floor(tmp(0)) + ((ispan(0, 23, 1)) / 24.)
  time_bnds(:, 1) = floor(tmp(0)) + ((ispan(1, 24, 1)) / 24.)
  delete(tmp)
  delete(tmp2)

  y->time_bnds = time_bnds


  ; Setup all of the time fields.
  field     = (/ "year", "month", "day", "tod", "jday", "jtod" /)
  nFields   = dimsizes(field)

  do i = 0, nFields-1
    tmp     = x->$field(i)$

    ovar      = new((/ 24 /), typeof(tmp))
    ovar!0    = "time"
    ovar&time = time

    copy_VarAtts(tmp, ovar)

    ; Most fields are the same every hour, except the time of day fields.
    ;
    ; For an hourly average, the time stamp should be in the middle of the time period
    if (field(i) .eq. "tod") then
      ovar(:) = (/ ispan(0, 23, 1) + 0.5 /)
    else
      if (field(i) .eq. "jtod") then
        ovar(:) = (/ ispan(0, 23, 1) * 3600 + 1800 /)
      else
        ovar(:) = tmp(0)
      end if
    end if

    delete(tmp)

    y->$field(i)$ = ovar
    delete(ovar)
  end do

  delete(field)


  ; Now average the flux data.
  field     = (/ "pflux", "eflux" /)
  nFields   = dimsizes(field)

  do i = 0, nFields-1

    ; Create new flux variables with just 24 times
    tmp   = x->$field(i)$

    edim  = tmp!1
    nedim = dimsizes(x->$edim$)

    ovar      = new((/ 24, nedim /), float)
    ovar!0    = "time"
    ovar&time = time
    ovar!1    = edim
    ovar&$edim$ = x->$edim$
    copy_VarAtts(tmp, ovar)
    ovar(:, :) = 0.

    ovarcnt    = new((/ 24, nedim /), integer)
    ovarcnt@_FillValue = -999
    ovarcnt(:, :) = 0

    ; Iterate over all of the measurements.
    ihr   = tointeger(floor(x->tod / 100))
    niTimes = dimsizes(ihr)
    do j = 0, niTimes-1
      do k = 0, nedim-1
        if (.not. ismissing(tmp(j, k))) then
          ovar(ihr(j), k) = ovar(ihr(j), k) + tmp(j, k)
          ovarcnt(ihr(j), k) = ovarcnt(ihr(j), k) + 1
        end if
      end do
    end do

    delete(tmp)

    ; Calculate average and put missing in if 0 or missing.
    ovarcnt = where(ovarcnt .eq. 0, ovarcnt@_FillValue, ovarcnt)

    do j = 0, 23
      do k = 0, nedim-1
        ovar(j, k) = ovar(j, k) / ovarcnt(j,k)
      end do
    end do

    ; Write out the average
    y->$field(i)$ = ovar

    delete(ovar)
    delete(ovarcnt)
  end do
end


; Average the data to 1 day.
;
; NOTE: Don't make assumptions about the time spacing in the original
; file, but assume that it is only 1 day per file.
undef("spe_2daily")
procedure spe_2daily(sat, prefix, year, month, day)

local i, j

begin

  ; Read in the source data.
  ifile  = "noaa_5m/" + year + "/" + "goes_" + year + month + day + sat + prefix + ".nc"
  if (isfilepresent(ifile)) then
    print("  " + ifile)
  else
    print("  skipping ... " + ifile)
    return
  end if
  x     = addfile(ifile, "r")

  ofile = "noaa_1d/" + year + "/" + "goes_" + year + month + day + sat + "_part_1d.nc"
  print("    " + ofile)
  system("mkdir -p noaa_1d/" + year)
  if (isfilepresent(ofile)) then
    system("rm " + ofile)
  end if
  y     = addfile(ofile, "c")
  filedimdef(y, "time", -1, True)

  ; Create an hourly time for this day.
  time = new((/ 1 /), double)
  time!0 = "time"

  tmp = x->time
  copy_VarAtts(tmp, time)
  time(0) = floor(tmp(0)) + 0.5
  delete(tmp)
  time&time = time

  y->time = time

  ; Create time bounds.
  time_bnds = new((/ 1, 2 /), double)
  time_bnds!0 = "time"
  time_bnds&time = time
  time_bnds!1 = "nbd"

  tmp = x->time_bnds
  copy_VarAtts(tmp, time)
  time_bnds(:, 0) = floor(time(0))
  time_bnds(:, 1) = floor(time(0)) + 1.
  delete(tmp)

  y->time_bnds = time_bnds


  ; Setup all of the time fields.
  field     = (/ "year", "month", "day", "tod", "jday", "jtod" /)
  nFields   = dimsizes(field)

  do i = 0, nFields-1
    tmp     = x->$field(i)$

    ovar      = new((/ 1 /), typeof(tmp))
    ovar!0    = "time"
    ovar&time = time

    copy_VarAtts(tmp, ovar)

    ; Most fields are the same every hour, except the time of day fields.
    ;
    ; For an hourly average, the time stamp should be in the middle of the time period
    if (field(i) .eq. "tod") then
      ovar(:) = 12.
    else
      if (field(i) .eq. "jtod") then
        ovar(:) = 12 * 3600
      else
        ovar(:) = tmp(0)
      end if
    end if

    delete(tmp)

    y->$field(i)$ = ovar
    delete(ovar)
  end do

  delete(field)


  ; Now average the flux data.
  field     = (/ "pflux", "eflux" /)
  nFields   = dimsizes(field)

  do i = 0, nFields-1

    ; Create new flux variables with just 24 times
    tmp   = x->$field(i)$


    edim    = tmp!1
    nedim  = dimsizes(x->$edim$)

    ovar      = new((/ 1, nedim /), float)
    ovar!0    = "time"
    ovar&time = time
    ovar!1    = edim
    ovar&$edim$ = x->$edim$
    copy_VarAtts(tmp, ovar)
    ovar(:, :) = 0.

    ovarcnt    = new((/ 1, nedim /), integer)
    ovarcnt@_FillValue = -999
    ovarcnt(:, :) = 0

    ; Iterate over all of the measurements.
    niTimes = dimsizes(x->time)
    do j = 0, niTimes-1
      do k = 0, nedim-1
        if (.not. ismissing(tmp(j, k))) then
          ovar(0, k) = ovar(0, k) + tmp(j, k)
          ovarcnt(0, k) = ovarcnt(0, k) + 1
        end if
      end do
    end do

    delete(tmp)

    ; Calculate average and put missing in if 0 or missing.
    ovarcnt = where(ovarcnt .eq. 0, ovarcnt@_FillValue, ovarcnt)

    do k = 0, nedim-1
      ovar(0, k) = ovar(0, k) / ovarcnt(0,k)
    end do

    ; Write out the average
    y->$field(i)$ = ovar

    delete(ovar)
    delete(ovarcnt)
  end do
end


; Average the hourly ion production data (not the proton fluxes) to daily.
undef("spe_hions2daily")
procedure spe_hions2daily(format, sat, year)

local i, j, x, y, p, prod, dprod

begin

  prefix = spe_prefix4format(format)

  ; Read in the source data.
  ifile  = "ions_1h/" + prefix + "/" + year + "/" + "ions_" + prefix + "_" + year + sat + "_part_1h" + ".nc"
  if (isfilepresent(ifile)) then
    print("  " + ifile)
  else
    print("  skipping ... " + ifile)
    return
  end if
  x     = addfile(ifile, "r")

  ofile = "ions_1d/" + prefix + "/" + year + "/" + "ions_" + prefix + "_" + year + sat + "_part_1d.nc"
  print("    " + ofile)
  system("mkdir -p ions_1d/" + prefix + "/" + year)
  if (isfilepresent(ofile)) then
    system("rm " + ofile)
  end if
  y     = addfile(ofile, "c")
  filedimdef(y, "time", -1, True)

  ; Step through by day.
  nTimes = getdimsize(x, "time")
  idate = ispan(0,nTimes-1,24)

  ; How the files are handled is dependent on the file format.
  if (format .le. 2) then

    ; Write out the fields that don't need to be averaged.
    y->date = x->date(idate)

    datesec = x->datesec(idate)
    datesec(:) = 12 * 3600
    y->datesec = datesec
    delete(datesec)

    if (format .eq. 2) then
      time = x->time(idate)
      time = floor(time)

      bnds = x->time_bnds(idate,:)
      bnds(:, 0) = (/ time /)
      bnds(:, 1) = (/ time + 1. /)

      time = (/ time + 0.5 /)
      y->time = time
      bnds&time = time
      y->time_bnds = bnds

      delete(bnds)
    end if

    prod = x->Prod
    dprod = prod(idate, :)

    if (format .eq. 2) then
      dprod&time = time
      delete(time)
    end if

    nDate = dimsizes(idate)
    do i = 0, nDate-1
      dprod(i, :) = dim_avg_n_Wrap(prod(24*i:(24*i)+23, :), 0)
    end do
    delete(prod)

    y->Prod = dprod

    delete(dprod)
  end if

  if (format .eq. 3) then
    time = x->time(idate)
    time = floor(time)

    bnds = x->time_bnds(idate,:)
    bnds(:, 0) = (/ time /)
    bnds(:, 1) = (/ time + 1. /)

    time = (/ time + 0.5 /)
    y->time = time
    bnds&time = time
    y->time_bnds = bnds

    delete(bnds)

    tmp  = x->calyear(idate)
    tmp&time = time
    y->calyear  = tmp
    delete(tmp)

    tmp  = x->calmonth(idate)
    tmp&time = time
    y->calmonth  = tmp
    delete(tmp)

    tmp  = x->calday(idate)
    tmp&time = time
    y->calday  = tmp
    delete(tmp)

    prod = x->iprp
    dprod = prod(idate, :)
    dprod&time = time
    delete(time)

    nDate = dimsizes(idate)
; TBD - Convert fill to 0 before average?
    do i = 0, nDate-1
      dprod(i, :) = dim_avg_n_Wrap(prod(24*i:(24*i)+23, :), 0)
    end do
    delete(prod)

    y->iprp = dprod

    delete(dprod)
  end if
end


; Calculate the ion production rates for all of the proton fluxes
; in the input file.
;
; NOTE: If month = "" and day = "", then it looks for a yearly file.
undef("spe_catyear")
procedure spe_catyear(sat, interval, year)

begin

  ifile = "noaa_" + interval + "/" + year + "/" + "goes_" + year + "????" + sat + "_part_" + interval + ".nc"
  ofile = "noaa_" + interval + "/" + year + "/" + "goes_" + year + sat + "_part_" + interval + ".nc"

  print("  ncrcat -O " + ifile + " " + ofile)
  system("ncrcat -O " + ifile + " " + ofile)
end


; Calculate the ion production rates for all of the proton fluxes
; in the input file.
;
; NOTE: If month = "" and day = "", then it looks for a yearly file.
undef("spe_cations")
procedure spe_cations(format, sat, interval)

begin

  prefix = spe_prefix4format(format)

  ifile = "ions_" + interval + "/" + prefix + "/*/ions_" + prefix + "_" + "????" + sat + "_part_" + interval + ".nc"
  ofile = "ions_" + interval + "/" + prefix + "/ions_" + prefix + sat + "_part_" + interval + ".nc"

  print("  ncrcat -O " + ifile + " " + ofile)
  system("ncrcat -O " + ifile + " " + ofile)
end


; Calculate the ion production rates for all of the proton fluxes
; in the input file.
;
; NOTE: If month = "" and day = "", then it looks for a yearly file.
undef("spe_calcions")
procedure spe_calcions(quiet, format, sat, interval, year, month, day)

begin

  prefix = spe_prefix4format(format)

  ; Read in the source data.
  if (day .eq. "") then

    ; Process a monthly file
    ifile = "noaa_" + interval + "/" + year + "/" + "goes_" + year + month + sat + "_part_" + interval + ".nc"
    ofile = "ions_" + interval + "/" + prefix + "/" + year + "/" + "ions_" + prefix + "_" + year + month + sat + "_part_" + interval + ".nc"
  else
    if (month .eq. "") then

      ; Process a yearly file
      ifile = "noaa_" + interval + "/" + year + "/" + "goes_" + year + sat + "_part_" + interval + ".nc"
      ofile = "ions_" + interval + "/" + prefix + "/" + year + "/" + "ions_" + prefix + "_" + year + sat + "_part_" + interval + ".nc"
    else

      ; Process a daily file
      ifile = "noaa_" + interval + "/" + year + "/" + "goes_" + year + month + day + sat + "_part_" + interval + ".nc"
      ofile = "ions_" + interval + "/" + prefix + "/" + year + "/" + "ions_" + prefix + "_" + year + month + day + sat + "_part_" + interval + ".nc"
    end if
  end if

  system("mkdir -p ions_" + interval + "/" + prefix + "/" + year)

  ; Call the go_proton fortran program to calculate the ion production rate.
  print("  " + ifile)
  print("    " + format + "  " + ofile)

  if (quiet) then
    system("bin/go_proton " + ifile + " " + ofile + " " + format + " >& /dev/null")
  else
    system("bin/go_proton " + ifile + " " + ofile + " " + format)
  end if
end


; Create an nc file with the sample data defined by Charley Jackman to test
; the go_proton program.
undef("spe_createSample")
procedure spe_createSample(filename)

begin

  time           = greg2jul(tointeger(data(:, 0)), tointeger(data(:, 1)), tointeger(data(:, 2)), hours) - greg2jul(1850, 1, 1, 0) + (minutes / 24. / 60.)
  time!0         = "time"
  time&time      = time
  time@long_name = "Time"
  time@unit      = "days since 1850-01-01 00:00:00"
  time@calendar  = "gregorian"
  nTimes         = dimsizes(time)

  ; Create a time bounds variable;
  ;
  ; NOTE: Assume that the data is evenly spaced.
  time_bnds           = new((/ nTimes, 2 /), double)
  time_bnds!0         = "time"
  time_bnds&time      = time
  time_bnds!1         = "nbd"
  time_bnds@long_name = "bounds of time bin"
  time_bnds@unit      = "days since 1850-01-01 00:00:00"
  time_bnds@calendar  = "gregorian"

  dtime               = time(1) - time(0)
  time_bnds(:, 0)     = time(:) - (dtime / 2.)
  time_bnds(:, 1)     = time(:) + (dtime / 2.)

  ; Create the output file.
  system("mkdir -p noaa_5m/" + year)
  ncfile   = "noaa_5m/" + year + "/" + "goes_" + year + month + day + osat + "_part_5m.nc"
  print("    " + ncfile)
  if (isfilepresent(ncfile)) then
    system("rm " + ncfile)
  end if
  ofile    = addfile(ncfile, "c")
  filedimdef(ofile, "time", -1, True)

  ; Write out the time and time bounds.
  ofile->time      = time
  ofile->time_bnds = time_bnds

  ; Write the output fields.
  do i = 0, nFields-1

    ; Create the proper output field type and set the longname and units.
    ; Pack proton and electron fluxes into 1 variable each.
    if (field(i) .eq. "pflux") then
      ovar            = new((/ nTimes, nPenergy /), type(i))
      ovar!0          = "time"
      ovar!1          = "penergy"
      ovar&penergy    = penergy
      ovar@_FillValue = fillvalue

      do j = 0, nPenergy-1
        ovar(:, j) = (/ data(:, offset(i) + j) /)
      end do
    else
      if (field(i) .eq. "eflux") then
        ovar            = new((/ nTimes, nEenergy /), type(i))
        ovar!0          = "time"
        ovar!1          = "eenergy"
        ovar&eenergy    = eenergy
        ovar@_FillValue = fillvalue

        do j = 0, nEenergy-1
          ovar(:, j) = (/ data(:, offset(i) + j) /)
        end do
      else
        if (type(i) .eq. "integer") then
          ovar = (/ tointeger(data(:, offset(i))) /)
        else
          ovar            = (/ data(:, offset(i)) /)
          ovar@_FillValue = fillvalue
        end if

        ovar!0          = "time"
      end if
    end if

    ovar&time         = time
    ovar@long_name    = longname(i)
    ovar@units        = unit(i)

    ofile->$field(i)$ = ovar

    delete(ovar)
  end do

end


; Average the data to 1 hr.
;
; NOTE: Don't make assumptions about the time spacing in the original
; file, but assume that it is only 1 day per file.
undef("spe_check4fill")
procedure spe_check4fill(verbose, threshold, sat, prefix, year, month, day)

local i, j

begin

  ; Read in the source data.
  ifile  = "noaa" + prefix + "/" + year + "/" + "goes_" + year + month + day + sat + "_part" + prefix + ".nc"
  files = systemfunc("ls " + ifile)

  if (any(ismissing(files))) then
    print("  no file found for ... " + ifile)
    return
  end if

  nfiles = dimsizes(files)

  ; Iterate over each file and if they have file values in the pflux then
  ; report the number of fill
  do i = 0, nfiles-1
    x = addfile(files(i), "r")

    pflux = x->pflux

    if (any(ismissing(pflux))) then

      ; Check how many intervals have a missing value in one of the fields
      print("  " + files(i))

      tod = x->tod
      ntimes = dimsizes(tod)
      count = 0
      do j = 0, ntimes-1
        if (any(ismissing(pflux(j, :)))) then
          if (verbose) then
            print("    " + tod(j))
          end if
          count = count + 1
        end if
      end do

;      if (count .gt. threshold) then
        if (ntimes .lt. 288) then
           print("    missing " + count + " of " + ntimes + ", bad total")
        else
;           print("    missing " + count + " of " + ntimes)
        end if
;      end if
      delete(tod)
    end if

    delete(pflux)
  end do
end


; Get GOES data files from the NOAA NCEI repository.
;
;   wget http://satdat.ngdc.noaa.gov/sem/goes/data/new_avg/1986/01/goes06/netcdf/g06_hepad_5m_19860101_19860131.nc
;
; NOTE: They don't seem to have an ftp site.
undef("spe_wgetncei")
procedure spe_wgetncei(verbose, sat, prefix, year, month, day)

local i, j

begin

  ; Read in the source data.
  ifile  = "noaa" + prefix + "/" + year + "/" + "goes_" + year + month + day + sat + "_part" + prefix + ".nc"
  files = systemfunc("ls " + ifile)

  if (any(ismissing(files))) then
    print("  no file found for ... " + ifile)
    return
  end if

  nfiles = dimsizes(files)

  ; Iterate over each file and if they have file values in the pflux then
  ; report the number of fill
  do i = 0, nfiles-1
    x = addfile(files(i), "r")

    pflux = x->pflux

    if (any(ismissing(pflux))) then

      ; Check how many intervals have a missing value in one of the fields
      print("  " + files(i))

      tod = x->tod
      ntimes = dimsizes(tod)
      count = 0
      do j = 0, ntimes-1
        if (any(ismissing(pflux(j, :)))) then
          if (verbose) then
            print("    " + tod(j))
          end if
          count = count + 1
        end if
      end do

      if (count .gt. threshold) then
        if (ntimes .lt. 288) then
           print("    missing " + count + " of " + ntimes + ", bad total")
        else
           print("    missing " + count + " of " + ntimes)
        end if
      end if
      delete(tod)
    end if

    delete(pflux)
  end do
end



; Tests

; Charley's Test Case
;sat = spe_primary4date("2003", "10", "29")
;spe_noaa2nc(sat, "_Gp", "2003", "10", "29")
;spe_2hourly("_Gp", "_part_5m", "2003", "10", "29")
;spe_calcions(False, 1, "_Gp", "1h", "2003", "10", "29")
;spe_calcions(False, 2, "_Gp", "1h", "2003", "10", "29")
;spe_calcions(False, 3, "_Gp", "1h", "2003", "10", "29")

;format = 3
;sat = spe_primary4date("2014", "01", "01")
;spe_noaa2nc(sat, "_Gp", "2014", "01", "01")
;spe_2hourly("_Gp", "_part_5m", "2014", "01", "01")
;spe_2daily("_Gp", "_part_5m", "2014", "01", "01")
;spe_calcions(False, format, "_Gp", "5m", "2014", "01", "01")
;spe_calcions(False, format, "_Gp", "1h", "2014", "01", "01")
;spe_calcions(False, format, "_Gp", "1d", "2014", "01", "01")
;spe_catyear("_Gp", "1h", "2014")
;spe_hions2daily(format, "_Gp", "2014")
;spe_hions2daily(format, "_Gp", "1999")


; Do all of the files
processFiles = True   ; NOAA text -> NETCDF
getFiles     = True
calcIons     = True   ; NETCDF flux -> NETCDF Ion Production
combineFiles = False   ; Combine the annual files together

do year = 2020, 2020
  if (processFiles) then
    do month = 3, 3
      do day = 1,5

        if (getFiles) then
          getTxtFiles(sprinti("%0.4i", year), sprinti("%0.2i", month), sprinti("%0.2i", day))
        end if

        ; Determine the primary satellite.
        sat = spe_primary4date(sprinti("%0.4i", year), sprinti("%0.2i", month), sprinti("%0.2i", day))

        ; Read in the data file and calculate the appropriate averages.
        if (sat .ne. "") then
          spe_noaa2nc(sat, "_Gp", sprinti("%0.4i", year), sprinti("%0.2i", month), sprinti("%0.2i", day))
          spe_2hourly("_Gp", "_part_5m", sprinti("%0.4i", year), sprinti("%0.2i", month), sprinti("%0.2i", day))
;          spe_2daily("_Gp", "_part_5m", sprinti("%0.4i", year), sprinti("%0.2i", month), sprinti("%0.2i", day))
        end if
      end do
    end do

    ; Concatenate the files together into yearly files.
    spe_catyear("_Gp", "1h", sprinti("%0.4i", year))
;    spe_catyear("_Gp", "1d", sprinti("%0.4i", year))
  end if

  ; Calculate the ionization rates
  if (calcIons) then

    ; Output format (1=WACCM4, 2=WACCM6, 3=CMIP6)
    do format = 1, 3

      spe_calcions(False, format, "_Gp", "1h", sprinti("%0.4i", year), "", "")
;      spe_calcions(True, format, "_Gp", "1d", sprinti("%0.4i", year), "", "")

      ; Calculate the daily ion production rate from averaging the hourly files.
      spe_hions2daily(format, "_Gp", sprinti("%0.4i", year))
    end do
  end if
end do


; Combine all the years into one file
if (combineFiles) then
;  do format = 1, 3
  do format = 1, 13
    spe_cations(format, "_Gp", "1d")
    spe_cations(format, "_Gp", "1h")
  end do
end if

; Convert Charley's data into netcdf for comparison purposes.
;do year = 1963, 2015
;  print("  " + sprinti("%0.4i", year))
;  spe_ions2nc(sprinti("%0.4i", year))
;end do
;system("ncrcat -O ions_nc/*/*.nc ions_nc/IonPair-gm_1963-2015.nc")


;spe_check4fill(False, "_Gp", "_5m", "2003", "05", "11")
;do year = 1999, 2018
;  spe_check4fill(False, 6, "_Gp", "_5m", sprinti("%0.4i", year), "*", "")
;end do
