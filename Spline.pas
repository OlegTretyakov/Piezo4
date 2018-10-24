unit Spline;

interface
  uses gsl22;


  type

  TSplineType = (st_cspline, st_linear, st_polynomial, st_cspline_periodic, st_akima, st_akima_periodic, st_steffen, st_linearextrap);

  TSpline = class(TObject)
  private
   FCount : Word;
   FSType : TSplineType;
   vAcc : pgsl_interp_accel;
   FSpline : pgsl_spline;
   FX,
   FY : array of double;
  public
   constructor Create(ASize : Word; SType : TSplineType = st_cspline);
   destructor Destroy; override;
   function GetCount: Word;
   procedure AddXY(AX, AY : double);
   property Count : Word read GetCount;
   function MinSize : Word;
   function FirstX : double;
   function FirstY : double;
   function LastX : double;
   function LastY : double;
   function X(Index : Word) : double;
   function Y(Index : Word) : double; overload;
   function Y(AX : double) : double; overload;
   procedure Clear;
   procedure InitSpline;
 end;

  
implementation


{ TSpVector }
constructor TSpline.Create(ASize : Word; SType : TSplineType = st_cspline);
begin 
  inherited Create;
  vAcc := nil;
  FSpline := nil;
  FSType := SType;
  SetLength(FX, ASize);
  SetLength(FY, ASize);
  FCount := 0;
  vAcc := gsl_interp_accel_alloc();
  case FSType of
    st_cspline: FSpline := gsl_spline_alloc (gsl_interp_cspline, Size_t(ASize));
    st_linear: FSpline := gsl_spline_alloc (gsl_interp_linear, Size_t(ASize));
    st_polynomial: FSpline := gsl_spline_alloc (gsl_interp_polynomial, Size_t(ASize));
    st_cspline_periodic: FSpline := gsl_spline_alloc (gsl_interp_cspline_periodic, Size_t(ASize));
    st_akima: FSpline := gsl_spline_alloc (gsl_interp_akima, Size_t(ASize));
    st_akima_periodic: FSpline := gsl_spline_alloc (gsl_interp_akima_periodic, Size_t(ASize));
    st_steffen : FSpline := gsl_spline_alloc (gsl_interp_steffen, Size_t(ASize));
    st_linearextrap : FSpline := gsl_spline_alloc (gsl_interp_cspline, Size_t(ASize));
  end;
end;

destructor TSpline.Destroy;
begin
  Clear;
  try
    if assigned(vAcc) then
      gsl_interp_accel_free (vAcc);
    if assigned(FSpline) then
      gsl_spline_free (FSpline);
  finally
    inherited Destroy;
    SetLength(FX, 0);
    SetLength(FY, 0);
  end; 
end;

function TSpline.FirstX: double;
begin
  result := FX[0];
end;

function TSpline.FirstY: double;
begin
  result := FY[0];
end;

function TSpline.GetCount: Word;
begin
  result := FCount;
end;

procedure TSpline.Clear;
begin
  FCount := 0;
end;

procedure TSpline.AddXY(AX, AY: double);
begin
  if (FCount = Length(FX)) then
    exit;
  FX[FCount] := AX;
  FY[FCount] := AY;
  Inc(FCount);
end;

procedure TSpline.InitSpline;
begin
  if (FCount >= MinSize) then
    gsl_spline_init (FSpline, FX, FY, FCount)
  else
    self.Clear;
end;

function TSpline.LastX: double;
begin
  result := FX[FCount-1];
end;

function TSpline.LastY: double;
begin
  result := FY[FCount-1];
end;

function TSpline.MinSize: Word;
begin
  if assigned(FSpline) then
    result := gsl_spline_min_size(FSpline)
  else
    result := 0;
end;

function bsearch(x_array : array of double; x: double):word;
var
i, ilo, ihi : word;
begin
  ilo := Low(x_array);
  ihi := High(x_array);
  while (ihi > ilo + 1) do
  begin
    i := (ihi + ilo) div 2;
    if (x_array[i] > x) then
      ihi := i
    else
      ilo := i;
  end;
  result := ilo;
end;

function TSpline.Y(AX: double): double;
var
vK, vB, vY_lo, vY_hi, vX_lo, vX_hi : double;
vIdx : Word;
begin
  if assigned(vAcc) and assigned(FSpline)
  and (FCount >= MinSize) then
  begin
    if (FSType <> st_linearextrap) then
      result := gsl_spline_eval(FSpline, AX, vAcc)
    else
    begin
      if (AX >= FirstX) and (AX <= LastX) then
        result := gsl_spline_eval(FSpline, AX, vAcc)
      else
      begin
        if (AX > LastX) then
        begin
          vX_lo := LastX - (LastX - FirstX)/2;
          vX_hi := LastX;
          //vIdx := gsl_interp_accel_find(vAcc, FX, FCount, vX_lo);
          vIdx := bsearch(FX, vX_lo);
          vY_hi := LastY;
          vY_lo := FY[vIdx];
        end else
        begin
          vX_lo := FirstX;
          vX_hi := FirstX + (LastX - FirstX)/2;
          //vIdx := gsl_interp_accel_find(vAcc, FX, FCount, vX_hi);
          vIdx := bsearch(FX, vX_hi);
          vY_hi := FY[vIdx];
          vY_lo := FirstY;
        end;
        vK := (vY_hi - vY_lo) / (vX_hi - vX_lo);
        vB := vY_lo - vK*vX_lo;
        result := vK*AX+vB;
      end;
    end;
  end else
    Result := 0;
end;

function TSpline.X(Index: Word): double;
begin
  if Index < Length(FX) then
    result := FX[Index]
  else
    result := 0;
end;

function TSpline.Y(Index: Word): double;
begin
  if Index < Length(FY) then
    result := FY[Index]
  else
    result := 0;
end;

end.
