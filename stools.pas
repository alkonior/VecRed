unit STools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin,
  FPCanvas, TypInfo, LCL, SFigures, UScale;

{ Classes }

type
  PBrushS = ^TBrushStyle;
  PPenS = ^TPenStyle;
  SVariant = ^variant;
  TProc = procedure(Sender: TObject);


  TMyButton = class
    Button: TSpeedButton;
    Proc: TProc;
    procedure OnClick(Sender: TObject);
    constructor Create(PR: TProc; Panel: TPanel; i, top, left: integer; s: string);
    destructor Destroy(); override;
  end;

  { TProperty }

  TProperty = class
    Res: variant;
    Name: string;
    procedure OnChange(Sender: TObject); virtual;
    destructor Destroy; virtual; abstract;
  end;

  ArrayOfProperty = array of TProperty;

  TSpinProperty = class(TProperty)
    SpinLabel: TLabel;
    SpinEdit: TSpinEdit;
    procedure OnChange(Sender: TObject);
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
  end;

  TColorProperty = class(TProperty)
    Button: TColorButton;
    procedure OnChange(Sender: TObject);
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
  end;

  TPenStyleProperty = class(TProperty)
    PenStylesLabel: TLabel;
    PenStylesBox: TComboBox;
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
    procedure OnChange(Sender: TObject);
    procedure PenStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStylesLable: TLabel;
    BrushStylesBox: TComboBox;
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
    procedure OnChange(Sender: TObject);
    procedure BrushStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  { TTool }
  TFigureClass = class of TFigure;

  TTool = class
    Figure: TFigureClass;
    Icon: string;
    IsMainTool: boolean;
    Number: integer;
    Propertys: ArrayOfProperty; static;
    procedure FigureCreate(Point: TFloatPoint); virtual;
    procedure ChangePoint(Point: TFloatPoint); virtual;
    procedure AddPoint(Point: TFloatPoint); virtual;
    procedure MouseUp(Point: TFloatPoint); virtual;
    procedure FigureEnd(); virtual;
    procedure CreateParams(); virtual;
    procedure DeleteParams(); virtual;
  end;

  { STools }

  TMultylineTool = class(TTool)
  public
    PenTool: TMyButton; static;
    PolylineTool: TMyButton; static;
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TPenTool = class(TMultylineTool)
  public
    constructor Create(n: integer);
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TPolylineTool = class(TMultylineTool)
  public
    constructor Create(n: integer);
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
  end;


  TLineTool = class(TTool)
  public
    constructor Create(n: integer);
  end;

  TRectangleTool = class(TTool)
  public
    constructor Create(n: integer);
  end;

  TRoundRectTool = class(TTool)
  public
    constructor Create(n: integer);
    procedure CreateParams(); override;
  end;

  TEllipseTool = class(TTool)
  public
    constructor Create(n: integer);
  end;

  TZoomTool = class(TTool)
  public
    Spoint:TFloatPoint;
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TRectZoomTool = class(TTool)
  public
    constructor Create(n: integer);
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
  end;

  TSelectTool = class(TTool)
  public
    Delete: TMyButton;static;
    AllTop: TMyButton;static;
    Allbottom: TMyButton;static;
    Designator: TMyButton;static;
    Changepoints: TMyButton;static;
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TDesignatorTool = class(TSelectTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
  end;

  TChangePointsTool = class(TSelectTool)
  public
    SPoint:TFloatPoint;
    SPPoint: PFloatPoint;
    IsMoving: boolean;
    lminp, lmaxp: TFloatPoint;
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure DeleteParams(); override;
  end;

  TScrollTool = class(TTool)
  public
    SPoint:TFloatPoint;
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

procedure ChangeMainTool(Sender: TObject);
procedure DeleteFigures(Sender: TObject);
procedure DeleteLastFigure();
procedure AllUpFigures(Sender: TObject);
procedure AllDownFigures(Sender: TObject);
procedure ChangeDependentTool(Sender: TObject);
procedure MakeParams(cl: Tclass; var AP: ArrayOfProperty);
procedure MakeSelectParams(var AP: ArrayOfProperty);
var           { Var }
  Tools: array of TTool;
  AButtons: array of TMyButton;
  ShiftButtonState: boolean = False;
  PropertyPanel: TPanel;
  ChoosenTool: TTool;
  ColorPanelTool: TPanel;
  ButtonPanel: TPanel;

implementation

{ Porocedures }

procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

procedure DeleteLastFigure();
begin
    if Length(Figures) > 0 then
      if Figures[high(Figures)] is TRectZoom then
      begin
        FreeAndNil(Figures[High(Figures)]);
        SetLength(Figures, Length(Figures) - 1);
      end;
  Drawing := False;

end;

procedure MakeSelectParams(var AP: ArrayOfProperty);
var
  ParamNameList, ParamTypeList: array of string;
  f1: boolean;
  i, j, k, n: integer;
  p: PPropList;
begin
  for i:=0 to high(ap) do ap[i].Destroy;
  for i := 0 to length(Figures) - 1 do
  begin
    if Figures[i].Selected then
    begin
      n := GetPropList(Figures[i], p);
      f1 := True;
      for j := 0 to n - 1 do
      begin
        f1 := True;
        for k := 0 to length(ParamTypeList) - 1 do
        begin
          if ((p^[j]^.PropType^.Name = ParamTypeList[k]) and
            (p^[j]^.Name = ParamnameList[k])) then
            f1 := False;
        end;
        if f1 then
        begin
          SetLength(ParamTypeList, Length(ParamTypeList) + 1);
          ParamTypeList[high(ParamTypeList)] := p^[j]^.PropType^.Name;
          SetLength(ParamnameList, Length(ParamnameList) + 1);
          ParamnameList[high(ParamnameList)] := p^[j]^.Name;
        end;
      end;
    end;
  end;
  SetLength(AP, LENGTH(ParamTypeList));
  for i := 0 to length(ParamTypeList) - 1 do
  begin
    case ParamTypeList[i] of
      'TGraphicsColor':
      begin
        Ap[i] := TColorProperty.Create(ParamNameList[i], i, ColorPanelTool);
      end;
      'LongInt':
      begin
        Ap[i] := TSpinProperty.Create(ParamNameList[i], i + 2, PropertyPanel);
      end;
      'TFPPenStyle':
      begin
        Ap[i] := TPenStyleProperty.Create(ParamNameList[i],
          i + 2, PropertyPanel);
      end;
      'TFPBrushStyle':
      begin
        Ap[i] := TBrushStyleProperty.Create(ParamNameList[i],
          i + 2, PropertyPanel);
      end;
    end;
  end;
  PropertyPanel.Height:=35*Length(ParamNameList);
end;


procedure MakeParams(cl: Tclass; var AP: ArrayOfProperty);
var
  n, i: integer;
  p: PPropList;
  s: string;
begin
  for i:=0 to high(ap) do
    ap[i].Destroy;
  n := GetPropList(cl, p);
  SetLength(AP, n);
  for i := 0 to n - 1 do
  begin
    s := p^[i]^.PropType^.Name;
    case s of
      'TGraphicsColor':
      begin
        Ap[i] := TColorProperty.Create(p^[i]^.Name, i, ColorPanelTool);
      end;
      'LongInt':
      begin
        Ap[i] := TSpinProperty.Create(p^[i]^.Name, i, PropertyPanel);
      end;
      'TFPPenStyle':
      begin
        Ap[i] := TPenStyleProperty.Create(p^[i]^.Name, i, PropertyPanel);
      end;
      'TFPBrushStyle':
      begin
        Ap[i] := TBrushStyleProperty.Create(p^[i]^.Name, i, PropertyPanel);
      end;
    end;
  end;
end;


procedure ChangeDependentTool(Sender: TObject);

begin
  IsShowPoits := False;
  Drawing:=false;
  if (ChoosenTool <> Tools[(Sender as TSpeedButton).tag]) then
    ChoosenTool := Tools[(Sender as TSpeedButton).Tag];
  if ChoosenTool.Number=12 then IsShowPoits:= SelectedNumber>0;
  InvalidateHandler;
end;

procedure ChangeMainTool(Sender: TObject);
var
  i: TFigure;
begin
  if (ChoosenTool <> Tools[(Sender as TSpeedButton).tag]) then
  begin
    ChoosenTool.FigureEnd();
    for i in figures do
      i.Selected := False;
    SelectedNumber := 0;
    ChoosenTool.DeleteParams();
    ChoosenTool := Tools[(Sender as TSpeedButton).Tag];
    ChoosenTool.CreateParams();
    Drawing := False;
    IsShowPoits := False;
  end;
  InvalidateHandler;
end;

procedure DeleteFigures(Sender: TObject);
var
  i, j: integer;
begin
  if SelectedNumber > 0 then
  begin
    j := 0;
    for i := 0 to Length(Figures) - 1 do
    begin
      if Figures[i].Selected then
      begin
        FreeAndNil(Figures[i]);
      end
      else
      begin
        Figures[j] := Figures[i];
        Inc(j);
      end;
    end;
    SetLength(Figures, j);
    SelectedNumber := 0;
  end;
  MakeSelectParams(ChoosenTool.Propertys);
  InvalidateHandler;
end;

procedure AllUpFigures(Sender: TObject);
var
  f: TFigure;
  i, j: integer;
begin
  for j := 1 to high(Figures) do
    for i := High(figures) downto Low(figures) do
    begin
      if Figures[i].Selected and (i + 1 < Length(Figures)) then
      begin
        f := Figures[i + 1];
        Figures[i + 1] := Figures[i];
        Figures[i] := F;
      end;
    end;
  InvalidateHandler;
end;

procedure AllDownFigures(Sender: TObject);
var
  f: TFigure;
  i, j: integer;
begin
  for j := 1 to high(Figures) do
    for i := low(figures) to high(figures) do
    begin
      if Figures[i].Selected and (i > 0) then
      begin
        f := Figures[i - 1];
        Figures[i - 1] := Figures[i];
        Figures[i] := F;
      end;
    end;
  InvalidateHandler;
end;

{ BoxDrowItem }

procedure TPenStyleProperty.PenStylesBoxDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  Y: integer;
begin
  Y := ARect.Top + 7;
  (Control as TComboBox).Canvas.Pen.Style := CasePenStyle(Index);
  (Control as TComboBox).Canvas.Line(0, Y, 200, Y);
end;

procedure TBrushStyleProperty.BrushStylesBoxDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  PRect: TRect;
begin
  PRect.Left := ARect.Left + 8;
  PRect.Right := ARect.Right - 8;
  PRect.Top := ARect.Top + 4;
  PRect.Bottom := ARect.Bottom - 4;
  (Control as TComboBox).Canvas.Brush.Style := CaseBrushStyle(Index);
  (Control as TComboBox).Canvas.Brush.Color := clBlack;
  (Control as TComboBox).Canvas.Rectangle(PRect);
end;

{ FigureCreate }

procedure TTool.FigureCreate(Point: TFloatPoint);
var i: Integer;
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := Figure.Create;
  Figures[High(Figures)].SetLengthPoints(2);
  Figures[High(Figures)].Points[0] := Point;
  Figures[High(Figures)].Points[1] := Point;
  for i :=0 to high(Propertys) do
  begin
    SetPropValue(Figures[High(Figures)],Propertys[i].Name,Propertys[i].Res);
  end;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TMultylineTool.FigureCreate(Point: TFloatPoint);
begin
  inherited;
  Figures[High(Figures)].SetLengthPoints(4);
  Figures[High(Figures)].Points[2] := Point;
  Figures[High(Figures)].Points[3] := Point;
end;


procedure TZoomTool.FigureCreate(Point: TFloatPoint);
begin
  spoint := WorldToScrn(point);
  if not (ShiftButtonState) then
    zoom := round(zoom * 1.2)
  else
    zoom := round(zoom / 1.2);
  if zoom > 1000 then
    zoom := 1000;
  if zoom < 0 then
    zoom := 1;
  Offset := Point * (zoom / 100) - spoint;
  spoint := ScrnToWorld(spoint);
end;

procedure TSelectTool.FigureCreate(Point: TFloatPoint);
var
  i: TFigure;
  j: Integer;
begin
for i in figures do
    i.Selected := False;
  SelectedNumber:=0;
  for j:=0 to high(Propertys) do Propertys[j].Destroy;
  SetLength(Propertys,0);
  inherited;
end;

procedure TDesignatorTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := Figure.Create;
  Figures[High(Figures)].SetLengthPoints(2);
  Figures[High(Figures)].Points[0] := Point;
  Figures[High(Figures)].Points[1] := Point;
end;

procedure TChangePointsTool.FigureCreate(Point: TFloatPoint);
var
  i: TFigure;
begin
  IsMoving := False;
  SPPoint := nil;
  if SelectedNumber = 0 then
  begin
    inherited;
  end
  else
  begin
    for i in Figures do
      if i.Selected then
        if i.checkpoint(point) <> nil then
          SPPoint := i.checkpoint(point);
    IsMoving:=not(SPPoint <> nil);
    SPoint := point;
    lminp := MinPoint;
    lmaxp := MaxPoint;
  end;
end;

procedure TScrollTool.FigureCreate(Point: TFloatPoint);
begin
  SPoint := Point;
end;


{ ChangePoint }

procedure TTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TMultylineTool.ChangePoint(Point: TFloatPoint);
begin
  if ShiftButtonState then
  begin
    MinPoint := min(MinPoint, point);
    MaxPoint := max(MaxPoint, point);
    with Figures[high(Figures)] do
    begin
      Figures[High(Figures)].SetLengthPoints(
        (length(Figures[High(Figures)].Points) + 1));
      points[high(points)] := point;
      Points[0] := min(Point, Points[0]);
      Points[1] := max(Point, Points[1]);
    end;
  end
  else
    with Figures[high(Figures)] do
    begin
      points[high(points)] := point;
    end;
end;

procedure TPenTool.ChangePoint(Point: TFloatPoint);
var b:Boolean;
begin
  b:=ShiftButtonState;
  ShiftButtonState:=True;
  inherited;
  ShiftButtonState:=b;
end;

procedure TPolylineTool.ChangePoint(Point: TFloatPoint);
var b:Boolean;
begin
  b:=ShiftButtonState;
  ShiftButtonState:=False;
  inherited;
  ShiftButtonState:=b;
end;

procedure TZoomTool.ChangePoint(Point: TFloatPoint);
begin
  if length(Figures) > 0 then
    Offset := Offset + WorldToScrn(spoint) - WorldToScrn(point);
end;

procedure TChangePointsTool.ChangePoint(Point: TFloatPoint);
var
  i: TFigure;
begin
  if SelectedNumber = 0 then
    inherited
  else
  begin
    if IsMoving then
    begin
      for i in Figures do
        if i.Selected then
          i.move((WorldToScrn(point) - WorldToScrn(spoint)) * 100 / zoom);
      MinPoint := Min(min(i.Points[0], i.Points[1]), lminp);
      MaxPoint := Max(max(i.Points[0], i.Points[1]), lmaxp);
      InvalidateHandler;
      SPoint := point;
    end
    else
    begin
      SPPoint^ := SPPoint^ + (WorldToScrn(point) - WorldToScrn(spoint)) * 100 / zoom;
      SPoint := point;
      for i in Figures do
        if i.Selected then
          i.move(FloatPoint(0, 0));
      MinPoint := Min(SPPoint^, lminp);
      MaxPoint := Max(SPPoint^, lmaxp);
    end;
  end;
end;

procedure TScrollTool.ChangePoint(Point: TFloatPoint);
begin
  if length(Figures) > 0 then
    Offset := Offset + WorldToScrn(spoint) - WorldToScrn(point);
end;

{ AddPoint }
procedure TTool.AddPoint(Point: TFloatPoint);
begin
  Figures[high(Figures)].Points[1] := point;
  Drawing := False;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TMultylineTool.AddPoint(Point: TFloatPoint);
begin
  if not (ShiftButtonState) then
  begin
    MinPoint := min(MinPoint, point);
    MaxPoint := max(MaxPoint, point);
    with Figures[high(Figures)] as TPolyline do
    begin
      Figures[High(Figures)].SetLengthPoints(
        (length(Figures[High(Figures)].Points) + 1));
      points[high(points)] := point;
      Points[0] := min(Point, Points[0]);
      Points[1] := max(Point, Points[1]);
    end;
  end;
end;

procedure TPolylineTool.AddPoint(Point: TFloatPoint);
var b:boolean;
begin
  b:=ShiftButtonState;
  ShiftButtonState:=false;
  inherited;
  ShiftButtonState:=b;
end;

procedure TRectZoomTool.AddPoint(Point: TFloatPoint);
begin
  Figures[high(Figures)].Points[1] := point;
  Drawing := False;
  ZoomToRect(Figures[High(Figures)].Points[0], Figures[High(Figures)].Points[1]);
  FreeAndNil(Figures[High(Figures)]);
  SetLength(Figures, Length(Figures) - 1);
end;

{ MouseUp }

procedure TTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
  begin
    Figures[high(Figures)].Points[1] := point;
    Drawing := False;
  end;
end;

procedure TMultylineTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
    Drawing := False;
end;

procedure TPenTool.MouseUp(Point: TFloatPoint);
begin
  Drawing := False;
end;

procedure TZoomTool.MouseUp(Point: TFloatPoint);
begin
  Drawing := False;
end;

procedure TScrollTool.MouseUp(Point: TFloatPoint);
begin
  Drawing := False;
end;

procedure TRectZoomTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
    if Figures[high(Figures)] is TRectZoom then
    begin
      ZoomToRect(Figures[High(Figures)].Points[0], Figures[High(Figures)].Points[1]);
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
      Drawing := False;
    end;
end;

procedure TSelectTool.MouseUp(Point: TFloatPoint);
var
  i: integer;
begin
    if Figures[High(Figures)] is TRectZoom then
      begin
      if (Figures[High(Figures)].points[0] * Figures[High(Figures)].points[1]) < 4 then
        for i := Length(Figures) - 2 downto 0 do
        begin
          if Figures[i].PointInFigure(Point) then
          begin
            Figures[i].Selected := not (Figures[i].Selected);
            if Figures[i].Selected then
              SelectedNumber := SelectedNumber + 1
            else
              SelectedNumber := SelectedNumber - 1;
            break;
          end;
        end
      else
      begin
        for i := 0 to Length(Figures) - 2 do
        begin
          if Figures[i].FigureInrect(Figures[High(Figures)].points[1],
            Figures[High(Figures)].points[0]) then
          begin
            Figures[i].Selected := not (Figures[i].Selected);
            SelectedNumber := ifthen(Figures[i].Selected, SelectedNumber + 1,SelectedNumber - 1);
          end;
        end;
      end;
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
    end;
    MakeSelectParams(Propertys);
    Drawing := False;
end;

procedure TChangePointsTool.MouseUp(Point: TFloatPoint);
var
  i: integer;
begin
  if SelectedNumber = 0 then
    begin
       inherited;
    end;
   if SelectedNumber > 0 then
      begin
        IsShowPoits := True;
        InvalidateHandler;
      end;
  Drawing := False;
  IsMoving := False;
end;

{ FigureEnd }

procedure TTool.FigureEnd();
begin
  Drawing := False;
end;

procedure TMultylineTool.FigureEnd();
begin
  if Length(Figures) > 0 then
    if Figures[High(Figures)] is TPolyline then
      with Figures[High(Figures)] as TPolyline do
      begin
        Points[0] := min(Points[0], Points[High(Points)]);
        Points[1] := max(Points[1], Points[High(Points)]);
      end;
  Drawing := False;
end;

procedure TRectZoomTool.FigureEnd();
begin
 DeleteLastFigure();
end;

procedure TSelectTool.FigureEnd();
begin
 DeleteLastFigure();
end;


{ CreateParams }

procedure TTool.CreateParams();
begin
  MakeParams(Figure, Propertys);
  ButtonPanel.Height := 0;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

procedure TMultylineTool.CreateParams();
begin
  MakeParams(Figure, Propertys);
  PenTool := TMyButton.Create((@ChangeDependentTool), ButtonPanel,
    Number + 1, 5, 0, 'ico/pen.png');
  PolylineTool := TMyButton.Create((@ChangeDependentTool), ButtonPanel,
    Number + 2, 5, 35, 'ico/polyline.png');
  ButtonPanel.Height := 40;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

procedure TSelectTool.CreateParams();
var
  v: variant;
begin
  Delete := TMyButton.Create((@DeleteFigures), ButtonPanel, 0, 0, 5, 'ico/delete.png');
  Designator := TMyButton.Create((@ChangeDependentTool), ButtonPanel,
    Number + 1, 33, 5, 'ico/select2.png');
  Changepoints := TMyButton.Create((@ChangeDependentTool), ButtonPanel,
    Number + 2, 33, 5 + 33, 'ico/round.png');
  AllBottom := TMyButton.Create((@AllDownFigures), ButtonPanel, 0,
    0, 5 + 33, 'ico/allbottom.png');
  AllTop := TMyButton.Create((@AllUpFigures), ButtonPanel, 0, 0,
    5 + 33 * 2, 'ico/alltop.png');
  ButtonPanel.Height := 70;
  PropertyPanel.Height := 45 * Length(Propertys) + 45;
  ButtonPanel.Height := 70;
end;

procedure TRoundRectTool.CreateParams();
begin
  inherited;
  (Propertys[high(Propertys)]  as TSpinProperty).SpinEdit.Value:=100;
  (Propertys[high(Propertys)-1]  as TSpinProperty).SpinEdit.Value:=100;
end;

{ DeleteParams }
procedure TTool.DeleteParams();
var
  i: TProperty;
begin
  for i in Propertys do
    i.Destroy;
  SetLength(Propertys, 0);
end;

procedure TMultylineTool.DeleteParams();
var
  i: TProperty;
begin
  Inherited;
  PenTool.Destroy();
  PolylineTool.Destroy();
end;

procedure TSelectTool.DeleteParams();
begin
  Delete.Destroy();
  Allbottom.Destroy();
  AllTop.Destroy();
  Designator.Destroy();
  Changepoints.Destroy();
end;

procedure TChangepointsTool.DeleteParams();
begin
  inherited;
  IsShowPoits := False;
  InvalidateHandler;
end;

{ PRPCreateDestroy }

constructor TSpinProperty.Create(s: string; n: integer; Panel: Tpanel);
begin
  SpinLabel := TLabel.Create(Panel);
  SpinLabel.Caption := s;
  SpinLabel.Align := alTop;
  SpinLabel.Top := n * 100;
  SpinLabel.Parent := Panel;
  SpinEdit := TSpinEdit.Create(Panel);
  SpinEdit.Align := alTop;
  SpinEdit.Parent := Panel;
  SpinEdit.MinValue := 1;
  SpinEdit.MaxValue := 10000;
  SpinEdit.Value := 1;
  SpinEdit.Width := 130;
  SpinEdit.Top := n * 100 + 50;
  SpinEdit.Alignment := taLeftJustify;
  SpinEdit.OnChange := @OnChange;
  res := SpinEdit.Value;
  Name := s;
end;

constructor TColorProperty.Create(s: string; n: integer; Panel: Tpanel);
begin
  Button := TColorButton.Create(Panel);
  Button.Align := alTop;
  Button.Parent := Panel;
  Button.Width := 32;
  Button.Height := 32;
  Button.Top := n * 100 + 50;
  Button.OnColorChanged := @OnChange;
  res := Button.ButtonColor;
  Name := s;
end;

constructor TPenStyleProperty.Create(s: string; n: integer; Panel: TPanel);
begin
  PenStylesLabel := TLabel.Create(Panel);
  PenStylesLabel.Top := 100 * n;
  PenStylesLabel.Caption := s;
  PenStyleslabel.Align := alTop;
  PenStylesLabel.Parent := Panel;
  PenStylesBox := TComboBox.Create(Panel);
  PenStylesBox.Align := alTop;
  PenStylesBox.ReadOnly := True;
  PenStylesBox.Top := 100 * n + 50;
  PenStylesBox.Items.CommaText := ',,,,';
  PenStylesBox.Style := csOwnerDrawFixed;
  PenStylesBox.ItemIndex := 0;
  PenStylesBox.Width := 130;
  PenStylesBox.Parent := Panel;
  PenStylesBox.OnDrawItem := @PenStylesBoxDrawItem;
  PenStylesBox.OnChange := @OnChange;
  Res := CasePenStyle(0);
  Name := s;
end;

constructor TBrushStyleProperty.Create(s: string; n: integer; Panel: TPanel);
begin
  BrushStylesLable := TLabel.Create(Panel);
  BrushStylesLable.Top := 100 * n;
  BrushStylesLable.Caption := s;
  BrushStylesLable.Align := alTop;
  BrushStylesLable.Parent := Panel;
  BrushStylesBox := TComboBox.Create(Panel);
  BrushStylesBox.Items.CommaText := ',,,,,,,';
  BrushStylesBox.ReadOnly := True;
  BrushStylesBox.Top := 100 * n + 50;
  BrushStylesBox.Parent := Panel;
  BrushStylesBox.Width := 130;
  BrushStylesBox.Style := csOwnerDrawFixed;
  BrushStylesBox.ItemIndex := 0;
  BrushStylesBox.Align := alTop;
  BrushStylesBox.OnDrawItem := @BrushStylesBoxDrawItem;
  BrushStylesBox.OnChange := @OnChange;
  Res := CaseBrushStyle(0);
  Name := s;
end;

destructor TSpinProperty.Destroy;
begin
  FreeAndNil(SpinEdit);
  FreeAndNil(SpinLabel);
end;

destructor TPenStyleProperty.Destroy;
begin
  FreeAndNil(PenStylesBox);
  FreeAndNil(PenStylesLabel);
end;

destructor TBrushStyleProperty.Destroy;
begin
  FreeAndNil(BrushStylesBox);
  FreeAndNil(BrushStylesLable);
end;

destructor TColorProperty.Destroy;
begin
  FreeAndNil(Button);
end;

{ ToolCreate }

constructor TMultylineTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/multyline.png';
  IsMainTool := True;
  Figure := TPolyline;
end;

constructor TLineTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/line.png';
  IsMainTool := True;
  Figure := TLine;
end;

constructor TRectangleTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/rectangle.png';
  IsMainTool := True;
  Figure := TRectangle;
end;

constructor TEllipseTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/ellipse.png';
  IsMainTool := True;
  Figure := TEllipse;
end;

constructor TZoomTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/zoom.png';
  IsMainTool := True;
  Figure := TFigure;
end;

constructor TScrollTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/Scroll.png';
  IsMainTool := True;
  Figure := TFigure;
end;

constructor TRectZoomTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/rectzoom.png';
  IsMainTool := True;
  Figure := TRectZoom;
end;

constructor TRoundRectTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/RoundRectangle.png';
  IsMainTool := True;
  Figure := TRoundRect;
end;

constructor TSelectTool.Create(n: integer);
begin
  Number := n;
  Icon := 'ico/select.png';
  IsMainTool := True;
  Figure := TRectZoom;
end;

constructor TDesignatorTool.Create(n: integer);
begin
  Number := n;
  IsMainTool := False;
  Figure := TRectZoom;
end;

constructor TChangePointsTool.Create(n: integer);
begin
  Number := n;
  IsMainTool := False;
  Figure := TRectZoom;
end;

constructor TPenTool.Create(n: integer);
begin
  Number := n;
  IsMainTool := False;
  Figure := TPolyline;
end;

constructor TPolylineTool.Create(n: integer);
begin
  Number := n;
  IsMainTool := False;
  Figure := TPolyline;
end;

destructor TMyButton.Destroy();
begin
  FreeAndNil(Button);
end;

procedure TProperty.OnChange(Sender: TObject);
var i:TFigure;
begin
  if SelectedNumber>0 then begin
  for i in Figures do
  if i.Selected then
    if IsPublishedProp(i,Name) then
    SetPropValue(i,Name,res);
  end;
  InvalidateHandler;
end;

procedure TSpinProperty.OnChange(Sender: TObject);
begin
  Res := (Sender as TSpinEdit).Value;
  inherited;
end;

procedure TColorProperty.OnChange(Sender: TObject);
begin
  Res := (Sender as TColorButton).ButtonColor;
  inherited;
end;

procedure TBrushStyleProperty.OnChange(Sender: TObject);
begin
  Res := CaseBrushStyle((Sender as TComboBox).ItemIndex);
  inherited;
end;

procedure TPenStyleProperty.OnChange(Sender: TObject);
begin
  Res := CasePenStyle((Sender as TComboBox).ItemIndex);
  inherited;
end;

{ TMyButton }
constructor TMyButton.Create(PR: TProc; Panel: TPanel; i, top, left: integer; s: string);
var
  ToolIcon: TBitmap;
begin
  Button := TSpeedButton.Create(Panel);
  ToolIcon := TBitmap.Create;
  with TPicture.Create do
  begin
    LoadFromFile(s);
    ToolIcon.Assign(Graphic);
  end;
  Button.Transparent := True;
  ToolIcon.Transparent := True;
  Button.Glyph := ToolIcon;
  Button.Width := 32;
  Button.Height := 32;
  Button.Top := top;
  Button.Left := left;
  Button.Tag := i;
  Button.GroupIndex := 1;
  Button.Down := i = 0;
  Proc := PR;
  Button.OnClick := @OnClick;
  Button.Parent := Panel;
end;

procedure TMyButton.OnClick(Sender: TObject);
begin
  proc(Sender);
end;

initialization
  RegisterTool(TMultylineTool.Create(length(Tools)));
  RegisterTool(TPenTool.Create(length(Tools)));
  RegisterTool(TPolylineTool.Create(length(Tools)));
  RegisterTool(TLineTool.Create(length(Tools)));
  RegisterTool(TRectangleTool.Create(length(Tools)));
  RegisterTool(TEllipseTool.Create(length(Tools)));
  RegisterTool(TRoundRectTool.Create(length(Tools)));
  RegisterTool(TZoomTool.Create(length(Tools)));
  RegisterTool(TRectZoomTool.Create(length(Tools)));
  RegisterTool(TScrollTool.Create(length(Tools)));
  RegisterTool(TSelectTool.Create(length(Tools)));
  RegisterTool(TDesignatorTool.Create(length(Tools)));
  RegisterTool(TChangePointsTool.Create(length(Tools)));
end.
