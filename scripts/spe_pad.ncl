; pad out the spe file with zeros for a certain number of days

procedure padspew(ifile, ofile, ndays)

begin

  system("rm " + ofile)
  system("cp " + ifile + " " + ofile)
  
  x = addfile(ofile, "w")
  
  time      = x->time
  date      = x->date
  datesec   = x->datesec
  time_bnds = x->time_bnds
  prod      = x->Prod
  
  nTimes    = dimsizes(time)
  print(nTimes)
  
  atime    = new(ndays, typeof(time))
  adate    = new(ndays, typeof(date))
  adatesec = new(ndays, typeof(datesec))
  atime_bnds = new((/ ndays, 2 /), typeof(time_bnds))
  aprod    = new((/ ndays, 58 /), typeof(prod))
  
  atime(0)    = time(nTimes-1) + 1.d0
  atime_bnds(0, 0) = time_bnds(nTimes-1, 0) + 1.d0
  atime_bnds(0, 1) = time_bnds(nTimes-1, 1) + 1.d0
  
  do i = 1, ndays-1
    atime(i)         = atime(i-1) + 1.d0
    atime_bnds(i, 0) = atime_bnds(i-1, 0) + 1.d0
    atime_bnds(i, 1) = atime_bnds(i-1, 1) + 1.d0
  end do
  
  adatesec(:) = 43200
  aprod(:,:)  = 0.
  
  ; "days since 1850-01-01 00:00:"
  agreg = jul2greg(atime(:) + greg2jul(1850, 1, 1, 0))
  adate(:) = agreg(:,0) * 10000 + agreg(:,1) * 100 + agreg(:,2)
  
  print(atime(:))
;  print(adate(:))
;  print(adatesec(:))
;  print(atime_bnds(0,:))
;  print(aprod(0,:))
  
  x->date = array_append_record(date, adate, 0)
  x->datesec = array_append_record(datesec, adatesec, 0)
  x->time_bnds = array_append_record(time_bnds, atime_bnds, 0)
  x->Prod = array_append_record(prod, aprod, 0)
  
  tmp = array_append_record(time, atime, 0)
  tmp&time = tmp
  printVarSummary(time)
  printVarSummary(atime)
  print(tmp)
  print(tmp&time)
  printVarSummary(tmp)
  x->time = tmp
end


procedure padspec(ifile, ofile, ndays)

begin

  system("rm " + ofile)
  system("cp " + ifile + " " + ofile)
  
  x = addfile(ofile, "w")
  
  time      = x->time
  calyear   = x->calyear
  calmonth  = x->calmonth
  calday    = x->calday
  time_bnds = x->time_bnds
  iprp      = x->iprp
  
  nTimes    = dimsizes(time)
  print(nTimes)
  
  atime     = new(ndays, typeof(time))
  acalyear  = new(ndays, typeof(calyear))
  acalmonth = new(ndays, typeof(calmonth))
  acalday   = new(ndays, typeof(calday))
  atime_bnds = new((/ ndays, 2 /), typeof(time_bnds))
  aiprp     = new((/ ndays, 58 /), typeof(iprp))
  
  atime(0)    = time(nTimes-1) + 1.d0
  atime_bnds(0, 0) = time_bnds(nTimes-1, 0) + 1.d0
  atime_bnds(0, 1) = time_bnds(nTimes-1, 1) + 1.d0
  
  do i = 1, ndays-1
    atime(i)         = atime(i-1) + 1.d0
    atime_bnds(i, 0) = atime_bnds(i-1, 0) + 1.d0
    atime_bnds(i, 1) = atime_bnds(i-1, 1) + 1.d0
  end do
  
  aiprp(:,:)  = 0.
  
  ; "days since 1850-01-01 00:00:"
  agreg = jul2greg(atime(:) + greg2jul(1850, 1, 1, 0))
  acalyear(:)  = agreg(:,0)
  acalmonth(:) = agreg(:,1)
  acalday(:)   = agreg(:,2)
    
  x->calyear = array_append_record(calyear, acalyear, 0)
  x->calmonth = array_append_record(calmonth, acalmonth, 0)
  x->calday = array_append_record(calday, acalday, 0)
  x->time_bnds = array_append_record(time_bnds, atime_bnds, 0)
  x->iprp = array_append_record(iprp, aiprp, 0)
  
  tmp = array_append_record(time, atime, 0)
  tmp&time = tmp
  x->time = tmp
end


;padspew("ions_w6_Gp_1999-2020_part_1d_c201005.nc", "ions_w6_Gp_1999-2020pad_part_1d_c210723.nc", 365)

padspec("ions_c6_Gp_1999-2020_part_1d_c201005.nc", "ions_c6_Gp_1999-2020pad_part_1d_c210723.nc", 365)
