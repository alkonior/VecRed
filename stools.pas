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
    destructor Destroy; virtual; abstract; overload;
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

  { TPropertyForSelect }

  TPropertyForSelect = class
    Res: variant;
    destructor Destroy; virtual; abstract; overload;
  end;

  ArrayOfPropertyForSelect = array of TPropertyForSelect;

  TSpinPropertyForSelect = class(TPropertyForSelect)
    SpinLabel: TLabel;
    SpinEdit: TSpinEdit;
    procedure OnChange(Sender: TObject);
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
  end;

  TColorPropertyForSelect = class(TPropertyForSelect)
    Button: TColorButton;
    procedure OnChange(Sender: TObject);
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
  end;

  TPenStylePropertyForSelect = class(TPropertyForSelect)
    PenStylesLabel: TLabel;
    PenStylesBox: TComboBox;
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
    procedure OnChange(Sender: TObject);
    procedure PenStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStylePropertyForSelect = class(TPropertyForSelect)
    BrushStylesLable: TLabel;
    BrushStylesBox: TComboBox;
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
    procedure OnChange(Sender: TObject);
    procedure BrushStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  { TTool }

  TTool = class
    Figure: TClass;
    Icon: string;
    IsMainTool: boolean;
    Number: integer;
    Propertys: ArrayOfProperty;
    procedure FigureCreate(Point: TFloatPoint); virtual; abstract;
    procedure ChangePoint(Point: TFloatPoint); virtual; abstract;
    procedure AddPoint(Point: TFloatPoint); virtual; abstract;
    procedure MouseUp(Point: TFloatPoint); virtual; abstract;
    procedure FigureEnd(); virtual; abstract;
    procedure CreateParams(); virtual; abstract;
    procedure DeleteParams(); virtual; abstract;
    procedure CreateParams(Tool: TTool); virtual; abstract;
  end;

  { STools }

  TMultylineTool = class(TTool)
  public
    PenTool: TMyButton;
    PolylineTool: TMyButton;
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
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(Tool: TTool); override;
    procedure DeleteParams(); override;
  end;

  TPolylineTool = class(TMultylineTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(Tool: TTool); override;
    procedure DeleteParams(); override;
  end;


  TLineTool = class(TTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TRectangleTool = class(TTool)
  public

    PRPBrushStyle: TBrushStyleProperty;
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TRoundRectTool = class(TTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TEllipseTool = class(TTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TZoomTool = class(TTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TRectZoomTool = class(TTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TSelectTool = class(TTool)
  public
    {ChosenFigure: TSpinProperty; }
    Delete: TMyButton;
    AllTop: TMyButton;
    Allbottom: TMyButton;
    Designator: TMyButton;
    Changepoints: TMyButton;
    APS: ArrayOfPropertyForSelect;
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TDesignatorTool = class(TSelectTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(Tool: TTool); override;
    procedure DeleteParams(); override;
  end;

  TChangePointsTool = class(TSelectTool)
  public
    STpoint: PFloatPoint;
    Moove: boolean;
    lminp, lmaxp: TFloatPoint;
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(Tool: TTool); override;
    procedure DeleteParams(); override;
  end;

  TScrollTool = class(TTool)
  public
    constructor Create(n: integer);
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

procedure ChangeMainTool(Sender: TObject);
procedure DeleteFigures(Sender: TObject);
procedure AllUpFigures(Sender: TObject);
procedure AllDownFigures(Sender: TObject);
procedure ChangeDependentTool(Sender: TObject);
procedure MakeParams(cl: Tclass; var AP: ArrayOfProperty);
procedure SendParams();
procedure DeleteSelectParams(var AP: ArrayOfPropertyForSelect);
{procedure MakeSelectedOneParams(F:TFigure; var AP: ArrayOfProperty); }
procedure MakeSelectParams(var AP: ArrayOfPropertyForSelect);

var           { Var }
  Tools: array of TTool;
  AButtons: array of TMyButton;
  SPoint: TFloatPoint;
  ShiftButtonState: boolean = False;
  PropertyPanel: TPanel;
  ChoosenTool: TTool;
  ColorPanelTool: TPanel;
  ButtonPanel: TPanel;

implementation

{ Porocedures }
procedure SendParams();
var
  i: TFigure;
  j: integer;
  l: tfigure;
  p: PPropList;
  n: integer;
begin
  for i in Figures do
    if i.Selected then
    begin
      case length((Tools[10] as TSelectTool).APS) of
        7: i.setprp((Tools[10] as TSelectTool).APS[0].res,
            (Tools[10] as TSelectTool).APS[1].res,
            (Tools[10] as TSelectTool).APS[2].res, (Tools[10] as TSelectTool).APS[3].res,
            (Tools[10] as TSelectTool).APS[4].res, (Tools[10] as TSelectTool).APS[5].res,
            (Tools[10] as TSelectTool).APS[6].res);
        5: i.setprp((Tools[10] as TSelectTool).APS[0].res,
            (Tools[10] as TSelectTool).APS[1].res,
            (Tools[10] as TSelectTool).APS[2].res, (Tools[10] as TSelectTool).APS[3].res,
            (Tools[10] as TSelectTool).APS[4].res, (Tools[10] as TSelectTool).APS[0].res,
            (Tools[10] as TSelectTool).APS[0].res);
        3: i.setprp((Tools[10] as TSelectTool).APS[0].res,
            (Tools[10] as TSelectTool).APS[1].res,
            (Tools[10] as TSelectTool).APS[2].res, (Tools[10] as TSelectTool).APS[0].res,
            (Tools[10] as TSelectTool).APS[0].res, (Tools[10] as TSelectTool).APS[0].res,
            (Tools[10] as TSelectTool).APS[0].res);
      end;
    end;
  InvalidateHandler;
end;

procedure DeleteSelectParams(var AP: ArrayOfPropertyForSelect);
var
  i: TPropertyForSelect;
begin
  for i in ap do
    i.Destroy();
  setlength(ap, 0);
end;

procedure MakeSelectParams(var AP: ArrayOfPropertyForSelect);
var
  ParamNameList, ParamTypeList: array of string;
  f1: boolean;
  i, j, k, n: integer;
  p: PPropList;
begin
  DeleteSelectParams(AP);
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
          continue;
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
        Ap[i] := TColorPropertyForSelect.Create(ParamNameList[i], i, ColorPanelTool);
      end;
      'LongInt':
      begin
        Ap[i] := TSpinPropertyForSelect.Create(ParamNameList[i], i + 2, PropertyPanel);
      end;
      'TFPPenStyle':
      begin
        Ap[i] := TPenStylePropertyForSelect.Create(ParamNameList[i],
          i + 2, PropertyPanel);
      end;
      'TFPBrushStyle':
      begin
        Ap[i] := TBrushStylePropertyForSelect.Create(ParamNameList[i],
          i + 2, PropertyPanel);
      end;
    end;
  end;
end;


procedure MakeParams(cl: Tclass; var AP: ArrayOfProperty);
var
  n, i: integer;
  p: PPropList;
  s: string;
  v: variant;
begin
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

{procedure MakeSelectedOneParams(F:TFigure; var AP: ArrayOfProperty);
var
  n, i: integer;
  p: PPropList;
  s: string;
  v: variant;
  b:boolean;
  pp:pointer;
begin
  n := GetPropList(f, p);
  SetLength(AP, n);
  for i := 0 to n - 1 do
  begin
    s := p^[i]^.PropType^.Name;
    case s of
      'TGraphicsColor':
      begin
        b:=IsStoredProp(f,p^[i]^.Name);
        pp:=GetRawInterfaceProp((f as f.ClassOfFigure),p^[i]);
        Ap[i] := TColorProperty.Create(p^[i]^.Name, i, ColorPanelTool, GetRawInterfaceProp((f as f.ClassOfFigure),p^[i]^.Name));
      end;
      'LongInt':
      begin
        Ap[i] := TSpinProperty.Create(p^[i]^.Name, i , PropertyPanel, GetRawInterfaceProp((f as f.ClassOfFigure),p^[i]^.Name));
      end;
      'TFPPenStyle':
      begin
        Ap[i] := TPenStyleProperty.Create(p^[i]^.Name, i , PropertyPanel, GetRawInterfaceProp((f as f.ClassOfFigure),p^[i]^.Name));
      end;
      'TFPBrushStyle':
      begin
        Ap[i] := TBrushStyleProperty.Create(p^[i]^.Name, i , PropertyPanel, GetRawInterfaceProp((f as f.ClassOfFigure),p^[i]^.Name));
      end;
    end;
  end;
end;     }

procedure ChangeDependentTool(Sender: TObject);
var
  t: TTool;
begin
  ShowPoits := False;
  if (ChoosenTool <> Tools[(Sender as TSpeedButton).tag]) then
  begin
    t := ChoosenTool;
    ChoosenTool := Tools[(Sender as TSpeedButton).Tag];
    (ChoosenTool).CreateParams((t));
  end;
  InvalidateHandler;
end;

procedure ChangeMainTool(Sender: TObject);
var
  i: TFigure;
begin
  ShowPoits := False;
  SelectedNumber := 0;
  if (ChoosenTool <> Tools[(Sender as TSpeedButton).tag]) then
  begin
    ChoosenTool.FigureEnd();
    for i in figures do
      i.Selected := False;
    InvalidateHandler;
    ChoosenTool.DeleteParams();
    ChoosenTool := Tools[(Sender as TSpeedButton).Tag];
    ChoosenTool.CreateParams();
    Drawing := False;
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
  InvalidateHandler;
end;

procedure AllUpFigures(Sender: TObject);
var
  f: TFigure;
  i: integer;
begin
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
  i: integer;
begin
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

procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

{ FigureCreate }

procedure TMultylineTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TPolyline.Create(Propertys[0].Res,
    Propertys[1].res, Propertys[2].res, point);
  Figures[High(Figures)].SetLengthPoints(4);
  Figures[High(Figures)].Points[3] := Point;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TPenTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TPolyline.Create(Propertys[0].Res,
    Propertys[1].res, Propertys[2].res, point);
  Figures[High(Figures)].SetLengthPoints(4);
  Figures[High(Figures)].Points[3] := Point;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TPolylineTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TPolyline.Create(Propertys[0].Res,
    Propertys[1].res, Propertys[2].res, point);
  Figures[High(Figures)].SetLengthPoints(4);
  Figures[High(Figures)].Points[3] := Point;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TLineTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TLine.Create(Propertys[0].Res, Propertys[1].res,
    Propertys[2].res, point);
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TEllipseTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(Propertys[0].Res,
    Propertys[1].Res, Propertys[2].res, Propertys[3].res, Propertys[4].Res, point);
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TRectangleTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(Propertys[0].Res,
    Propertys[1].Res, Propertys[2].res, Propertys[3].res, Propertys[4].Res, point);
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TRoundRectTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRoundRect.Create(Propertys[0].Res,
    Propertys[1].Res, Propertys[2].res, Propertys[3].res, Propertys[4].Res,
    Propertys[5].Res, Propertys[6].Res, point);
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
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

procedure TRectZoomTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectZoom.Create(point);
end;

procedure TSelectTool.FigureCreate(Point: TFloatPoint);
var
  i: TFigure;
begin
  if length(aps) > 0 then
    DeleteSelectParams(aps);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectZoom.Create(point);
  for i in figures do
    i.Selected := False;
end;

procedure TChangePointsTool.FigureCreate(Point: TFloatPoint);
var
  i: TFigure;
begin
  if length(aps) > 0 then
    DeleteSelectParams(aps);
  moove := False;
  STpoint := nil;
  if SelectedNumber = 0 then
  begin
    Setlength(Figures, length(figures) + 1);
    Figures[High(Figures)] := TRectZoom.Create(point);
  end
  else
  begin
    for i in Figures do
      if i.Selected then
        if i.checkpoint(point) <> nil then
          STpoint := i.checkpoint(point);
    if STpoint <> nil then
      moove := False
    else
      Moove := True;
    SPoint := point;
    lminp := MinPoint;
    lmaxp := MaxPoint;
  end;
end;

procedure TDesignatorTool.FigureCreate(Point: TFloatPoint);
begin
  if length(aps) > 0 then
    DeleteSelectParams(aps);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectZoom.Create(point);
end;

procedure TScrollTool.FigureCreate(Point: TFloatPoint);
begin
  SPoint := Point;
end;


{ ChangePoint }

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
begin
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
  with Figures[high(Figures)] do
  begin
    Figures[High(Figures)].SetLengthPoints((length(Figures[High(Figures)].Points) + 1));
    points[high(points)] := point;
    Points[0] := min(Point, Points[0]);
    Points[1] := max(Point, Points[1]);
  end;
end;

procedure TPolylineTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TLineTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TRectangleTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TRoundRectTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TEllipseTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TZoomTool.ChangePoint(Point: TFloatPoint);
begin
  if length(Figures) > 0 then
    Offset := Offset + WorldToScrn(spoint) - WorldToScrn(point);
end;

procedure TRectZoomTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TSelectTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TChangePointsTool.ChangePoint(Point: TFloatPoint);
var
  i: TFigure;
begin
  if SelectedNumber = 0 then
    with Figures[high(Figures)] do
    begin
      points[high(points)] := point;
    end
  else
  begin
    if Moove then
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
      STpoint^ := STpoint^ + (WorldToScrn(point) - WorldToScrn(spoint)) * 100 / zoom;
      SPoint := point;
      for i in Figures do
        if i.Selected then
          i.move(FloatPoint(0, 0));
      MinPoint := Min(STpoint^, lminp);
      MaxPoint := Max(STpoint^, lmaxp);
    end;

  end;
end;

procedure TDesignatorTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TScrollTool.ChangePoint(Point: TFloatPoint);
begin
  if length(Figures) > 0 then
    Offset := Offset + WorldToScrn(spoint) - WorldToScrn(point);
end;

{ AddPoint }

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
begin
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
  with Figures[high(Figures)] as TPolyline do
  begin
    Figures[High(Figures)].SetLengthPoints((length(Figures[High(Figures)].Points) + 1));
    points[high(points)] := point;
    Points[0] := min(Point, Points[0]);
    Points[1] := max(Point, Points[1]);
  end;
end;

procedure TPenTool.AddPoint(Point: TFloatPoint);
begin
end;

procedure TLineTool.AddPoint(Point: TFloatPoint);
begin
  Figures[high(Figures)].Points[1] := point;
  Drawing := False;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TRectangleTool.AddPoint(Point: TFloatPoint);
begin
  Figures[high(Figures)].Points[1] := point;
  Drawing := False;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TRoundRectTool.AddPoint(Point: TFloatPoint);
begin
  Figures[high(Figures)].Points[1] := point;
  Drawing := False;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TRectZoomTool.AddPoint(Point: TFloatPoint);
begin
  Figures[high(Figures)].Points[1] := point;
  Drawing := False;
  ZoomToRect(Figures[High(Figures)].Points[0], Figures[High(Figures)].Points[1]);
  FreeAndNil(Figures[High(Figures)]);
  SetLength(Figures, Length(Figures) - 1);
end;

procedure TSelectTool.AddPoint(Point: TFloatPoint);
var
  i: integer;
begin
  if ShiftButtonState then
  begin
    SelectedNumber := 0;
    if Figures[High(Figures)] is TRectZoom then
      if (Figures[High(Figures)].points[0] * Figures[High(Figures)].points[1]) < 4 then
        for i := Length(Figures) - 2 downto 0 do
        begin
          if Figures[i].PointInFigure(Point) then
          begin
            Figures[i].Selected := True;
            SelectedNumber := 1;
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
            Figures[i].Selected := True;
            SelectedNumber := SelectedNumber + 1;
          end;
        end;
      end;
    if Figures[high(Figures)] is TRectZoom then
    begin
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
    end;
    Drawing := False;
  end;
  if SelectedNumber > 0 then
  begin
    MakeSelectParams(APS);
    ButtonPanel.Height := 70;
    PropertyPanel.Height := 39 * Length(APS);
  end
  else
    DeleteSelectParams(aps);
end;

procedure TChangePointsTool.AddPoint(Point: TFloatPoint);
var
  i: integer;
begin
  if SelectedNumber = 0 then
    if ShiftButtonState then
    begin
      SelectedNumber := 0;
      if Figures[High(Figures)] is TRectZoom then
        if (Figures[High(Figures)].points[0] * Figures[High(Figures)].points[1]) < 4 then
          for i := Length(Figures) - 2 downto 0 do
          begin
            if Figures[i].PointInFigure(Point) then
            begin
              Figures[i].Selected := True;
              SelectedNumber := 1;
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
              Figures[i].Selected := True;
              SelectedNumber := SelectedNumber + 1;
            end;
          end;
        end;
      if Figures[high(Figures)] is TRectZoom then
      begin
        FreeAndNil(Figures[High(Figures)]);
        SetLength(Figures, Length(Figures) - 1);
      end;
      if SelectedNumber > 0 then
      begin
        ShowPoits := True;
        InvalidateHandler;
      end;
    end;
  Drawing := False;
  Moove := False;
  if SelectedNumber > 0 then
  begin
    MakeSelectParams(APS);
    ButtonPanel.Height := 70;
    PropertyPanel.Height := 39 * Length(APS);
  end
  else
    DeleteSelectParams(aps);
end;

procedure TDesignatorTool.AddPoint(Point: TFloatPoint);
var
  i: integer;
begin
  if ShiftButtonState then
  begin
    if Figures[High(Figures)] is TRectZoom then
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
            if Figures[i].Selected then
              SelectedNumber := SelectedNumber + 1
            else
              SelectedNumber := SelectedNumber - 1;
          end;
        end;
      end;
    if Figures[high(Figures)] is TRectZoom then
    begin
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
    end;
    Drawing := False;
  end;
  if SelectedNumber > 0 then
  begin
    MakeSelectParams(APS);
    ButtonPanel.Height := 70;
    PropertyPanel.Height := 39 * Length(APS);
  end
  else
    DeleteSelectParams(aps);
end;

procedure TEllipseTool.AddPoint(Point: TFloatPoint);
begin
  Figures[high(Figures)].Points[1] := point;
  Drawing := False;
end;

procedure TZoomTool.AddPoint(Point: TFloatPoint);
begin
end;

procedure TScrollTool.AddPoint(Point: TFloatPoint);
begin
end;

{ MouseUp }

procedure TMultylineTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
    Drawing := False;
end;

procedure TPenTool.MouseUp(Point: TFloatPoint);
begin
  Drawing := False;
end;

procedure TPolyLineTool.MouseUp(Point: TFloatPoint);
begin
end;

procedure TLineTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
  begin
    Figures[high(Figures)].Points[1] := point;
    Drawing := False;
  end;
end;

procedure TRectangleTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
  begin
    Figures[high(Figures)].Points[1] := point;
    Drawing := False;
  end;
end;

procedure TEllipseTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
  begin
    Figures[high(Figures)].Points[1] := point;
    Drawing := False;
  end;
end;

procedure TRoundRectTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
  begin
    Figures[high(Figures)].Points[1] := point;
    Drawing := False;
  end;
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
  if not (ShiftButtonState) then
  begin
    SelectedNumber := 0;
    if Figures[High(Figures)] is TRectZoom then
      if (Figures[High(Figures)].points[0] * Figures[High(Figures)].points[1]) < 4 then
        for i := Length(Figures) - 2 downto 0 do
        begin
          if Figures[i].PointInFigure(Point) then
          begin
            Figures[i].Selected := True;
            SelectedNumber := 1;
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
            Figures[i].Selected := True;
            SelectedNumber := SelectedNumber + 1;
          end;
        end;
      end;
    if Figures[high(Figures)] is TRectZoom then
    begin
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
    end;
    Drawing := False;
  end;
  if SelectedNumber > 0 then
  begin
    MakeSelectParams(APS);
    ButtonPanel.Height := 70;
    PropertyPanel.Height := 39 * Length(APS);
  end
  else
    DeleteSelectParams(aps);
end;

procedure TChangePointsTool.MouseUp(Point: TFloatPoint);
var
  i: integer;
begin
  if SelectedNumber = 0 then
    if not (ShiftButtonState) then
    begin
      SelectedNumber := 0;
      if Figures[High(Figures)] is TRectZoom then
        if (Figures[High(Figures)].points[0] * Figures[High(Figures)].points[1]) < 4 then
          for i := Length(Figures) - 2 downto 0 do
          begin
            if Figures[i].PointInFigure(Point) then
            begin
              Figures[i].Selected := True;
              SelectedNumber := 1;
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
              Figures[i].Selected := True;
              SelectedNumber := SelectedNumber + 1;
            end;
          end;
        end;
      if Figures[high(Figures)] is TRectZoom then
      begin
        FreeAndNil(Figures[High(Figures)]);
        SetLength(Figures, Length(Figures) - 1);
      end;
      if SelectedNumber > 0 then
      begin
        ShowPoits := True;
        InvalidateHandler;
      end;
    end;
  Drawing := False;
  Moove := False;
  if SelectedNumber > 0 then
  begin
    MakeSelectParams(APS);
    ButtonPanel.Height := 70;
    PropertyPanel.Height := 39 * Length(APS);
  end
  else
    DeleteSelectParams(aps);
end;

procedure TDesignatorTool.MouseUp(Point: TFloatPoint);
var
  i: integer;
begin
  if not (ShiftButtonState) then
  begin
    if Figures[High(Figures)] is TRectZoom then
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
            if Figures[i].Selected then
              SelectedNumber := SelectedNumber + 1
            else
              SelectedNumber := SelectedNumber - 1;
          end;
        end;
      end;
    if Figures[high(Figures)] is TRectZoom then
    begin
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
    end;
    Drawing := False;
  end;
  if SelectedNumber > 0 then
  begin
    MakeSelectParams(APS);
    ButtonPanel.Height := 70;
    PropertyPanel.Height := 39 * Length(APS);
  end
  else
    DeleteSelectParams(aps);
end;

procedure TZoomTool.MouseUp(Point: TFloatPoint);
begin
  Drawing := False;
end;

procedure TScrollTool.MouseUp(Point: TFloatPoint);
begin
  Drawing := False;
end;


{ FigureEnd }

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

procedure TPenTool.FigureEnd();
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


procedure TPolylineTool.FigureEnd();
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


procedure TLineTool.FigureEnd();
begin
  Drawing := False;
end;

procedure TRectangleTool.FigureEnd();
begin
  Drawing := False;
end;

procedure TRoundRectTool.FigureEnd();
begin
  Drawing := False;
end;

procedure TEllipseTool.FigureEnd();
begin
  Drawing := False;
end;

procedure TRectZoomTool.FigureEnd();
begin
  if Length(Figures) > 0 then
    if Figures[high(Figures)] is TRectZoom then
    begin
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
    end;
  Drawing := False;
end;

procedure TSelectTool.FigureEnd();
begin
  if SelectedNumber = 0 then
    if Length(Figures) > 0 then
      if Figures[high(Figures)] is TRectZoom then
      begin
        FreeAndNil(Figures[High(Figures)]);
        SetLength(Figures, Length(Figures) - 1);
      end;
  Drawing := False;
end;


procedure TChangepointsTool.FigureEnd();
begin
  if Length(Figures) > 0 then
    if Figures[high(Figures)] is TRectZoom then
    begin
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
    end;
  Drawing := False;
end;

procedure TDesignatorTool.FigureEnd();
begin
  if Length(Figures) > 0 then
    if Figures[high(Figures)] is TRectZoom then
    begin
      FreeAndNil(Figures[High(Figures)]);
      SetLength(Figures, Length(Figures) - 1);
    end;
  Drawing := False;
end;

procedure TZoomTool.FigureEnd();
begin
  Drawing := False;
end;

procedure TScrollTool.FigureEnd();
begin
  Drawing := False;
end;

{ CreateParams }

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

procedure TLineTool.CreateParams();
begin
  MakeParams(Figure, Propertys);
  PropertyPanel.Height := 77;
  ButtonPanel.Height := 0;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

procedure TRectangleTool.CreateParams();
begin
  MakeParams(Figure, Propertys);
  PropertyPanel.Height := 78 + 39 - 1;
  ButtonPanel.Height := 0;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

procedure TEllipseTool.CreateParams();
begin
  MakeParams(Figure, Propertys);

  ButtonPanel.Height := 0;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

procedure TRoundRectTool.CreateParams();
begin
  MakeParams(Figure, Propertys);
  ButtonPanel.Height := 0;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

procedure TRectZoomTool.CreateParams();
begin
  ButtonPanel.Height := 0;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

procedure TSelectTool.CreateParams();
var
  v: variant;
begin
  if length(aps) > 0 then
    SetLength(APS, 0);
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
end;

procedure TDesignatorTool.CreateParams(Tool: TTool);
var
  i, j: integer;
begin
  APS := (Tool as TSelectTool).APS;
  if length(aps) > 0 then
    DeleteSelectParams(aps);
  Delete := (Tool as TSelectTool).Delete;
  Designator := (Tool as TSelectTool).Designator;
  Changepoints := (Tool as TSelectTool).Changepoints;
  Allbottom := (Tool as TSelectTool).Allbottom;
  AllTop := (Tool as TSelectTool).AllTop;
  if SelectedNumber > 0 then
  begin
    MakeSelectParams(APS);
  end;
  ButtonPanel.Height := 70;
  PropertyPanel.Height := 39 * Length(APS);
end;

procedure TChangePointsTool.CreateParams(Tool: TTool);
begin
  APS := (Tool as TSelectTool).APS;
  if length(aps) > 0 then
    DeleteSelectParams(aps);
  Delete := (Tool as TSelectTool).Delete;
  Designator := (Tool as TSelectTool).Designator;
  Allbottom := (Tool as TSelectTool).Allbottom;
  AllTop := (Tool as TSelectTool).AllTop;
  Changepoints := (Tool as TSelectTool).Changepoints;
  if SelectedNumber > 0 then
  begin
    ShowPoits := True;
    InvalidateHandler;
  end;
  if SelectedNumber > 0 then
  begin
    MakeSelectParams(APS);
  end;
  ButtonPanel.Height := 70;
  PropertyPanel.Height := 39 * Length(APS);
end;

procedure TPolylineTool.CreateParams(Tool: TTool);
begin
  Propertys := tool.Propertys;
  PenTool := (Tool as TMultylineTool).PenTool;
  PolyLineTool := (Tool as TMultylineTool).PolyLineTool;
end;

procedure TPenTool.CreateParams(Tool: TTool);
begin
  PenTool := (Tool as TMultylineTool).PenTool;
  PolyLineTool := (Tool as TMultylineTool).PolyLineTool;
  Propertys := tool.Propertys;
end;

procedure TZoomTool.CreateParams();
begin
  ButtonPanel.Height := 0;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

procedure TScrollTool.CreateParams();
begin
  ButtonPanel.Height := 0;
  PropertyPanel.Height := 39 * Length(Propertys);
end;

{ DeleteParams }

procedure TMultylineTool.DeleteParams();
var
  i: TProperty;
begin
  PenTool.Destroy();
  PolylineTool.Destroy();
  for i in Propertys do
    i.Destroy;
  SetLength(Propertys, 0);
end;

procedure TLineTool.DeleteParams();
var
  i: TProperty;
begin
  for i in Propertys do
    i.Destroy;
  SetLength(Propertys, 0);
end;

procedure TRectangleTool.DeleteParams();
var
  i: TProperty;
begin
  for i in Propertys do
    i.Destroy;
  SetLength(Propertys, 0);
end;

procedure TRoundRectTool.DeleteParams();
var
  i: TProperty;
begin
  for i in Propertys do
    i.Destroy;
  SetLength(Propertys, 0);
end;

procedure TRectZoomTool.DeleteParams();
begin
end;

procedure TSelectTool.DeleteParams();
begin
  Delete.Destroy();
  Allbottom.Destroy();
  AllTop.Destroy();
  Designator.Destroy();
  Changepoints.Destroy();
  DeleteSelectParams(aps);
end;

procedure TDesignatorTool.DeleteParams();
begin
  Delete.Destroy();
  Allbottom.Destroy();
  AllTop.Destroy();
  Designator.Destroy();
  Changepoints.Destroy();
  DeleteSelectParams(aps);
end;

procedure TChangepointsTool.DeleteParams();
begin
  Delete.Destroy();
  Allbottom.Destroy();
  AllTop.Destroy();
  Designator.Destroy();
  Changepoints.Destroy();
  ShowPoits := False;
  InvalidateHandler;
  DeleteSelectParams(aps);
end;

procedure TPolylineTool.DeleteParams();
var
  i: TProperty;
begin
  for i in Propertys do
    i.Destroy;
  SetLength(Propertys, 0);
  PenTool.Destroy();
  PolylineTool.Destroy();
end;

procedure TPenTool.DeleteParams();
var
  i: TProperty;
begin
  for i in Propertys do
    i.Destroy;
  SetLength(Propertys, 0);
  PenTool.Destroy();
  PolylineTool.Destroy();
end;

procedure TEllipseTool.DeleteParams();
var
  i: TProperty;
begin
  for i in Propertys do
    i.Destroy;
  SetLength(Propertys, 0);
end;

procedure TZoomTool.DeleteParams();
begin
end;

procedure TScrollTool.DeleteParams();
begin
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
end;

constructor TColorProperty.Create(s: string; n: integer; Panel: Tpanel);
begin
  Button := TColorButton.Create(Panel);
  Button.Align := alTop;
  Button.Parent := Panel;
  Button.Width := 32;
  Button.Height := 32;
  Button.left := n * 100 + 50;
  Button.OnColorChanged := @OnChange;
  res := Button.ButtonColor;
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
  Figure := TPolyline;
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

destructor TMyButton.Destroy();
begin
  inherited Destroy;
  FreeAndNil(Button);
end;

procedure TSpinProperty.OnChange(Sender: TObject);
begin
  Res := (Sender as TSpinEdit).Value;
end;

procedure TColorProperty.OnChange(Sender: TObject);
begin
  Res := (Sender as TColorButton).ButtonColor;
end;

procedure TBrushStyleProperty.OnChange(Sender: TObject);
begin
  Res := CaseBrushStyle((Sender as TComboBox).ItemIndex);
end;

{  TPropertyForSelect }

procedure TpenStyleProperty.OnChange(Sender: TObject);
begin
  Res := CasePenStyle((Sender as TComboBox).ItemIndex);
  SendParams();
end;

procedure TSpinPropertyForSelect.OnChange(Sender: TObject);
begin
  Res := (Sender as TSpinEdit).Value;
  SendParams();
end;

procedure TColorPropertyForSelect.OnChange(Sender: TObject);
begin
  Res := (Sender as TColorButton).ButtonColor;
  SendParams();
end;

procedure TBrushStylePropertyForSelect.OnChange(Sender: TObject);
begin
  Res := CaseBrushStyle((Sender as TComboBox).ItemIndex);
  SendParams();
end;

procedure TpenStylePropertyForSelect.OnChange(Sender: TObject);
begin
  Res := CasePenStyle((Sender as TComboBox).ItemIndex);
  SendParams();
end;

{ PRPCreateDestroyForSelect }
constructor TSpinPropertyForSelect.Create(s: string; n: integer; Panel: Tpanel);
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
  Res := SpinEdit.Value;
end;

constructor TColorPropertyForSelect.Create(s: string; n: integer; Panel: Tpanel);
begin
  Button := TColorButton.Create(Panel);
  Button.Align := alTop;
  Button.Parent := Panel;
  Button.Width := 32;
  Button.Height := 32;
  Button.left := n * 100 + 50;
  Button.OnColorChanged := @OnChange;
  Res := Button.ButtonColor;
end;

constructor TPenStylePropertyForSelect.Create(s: string; n: integer; Panel: TPanel);
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
  res := CasePenStyle(0);
end;

constructor TBrushStylePropertyForSelect.Create(s: string; n: integer; Panel: TPanel);
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
end;

destructor TSpinPropertyForSelect.Destroy;
begin
  FreeAndNil(SpinEdit);
  FreeAndNil(SpinLabel);
end;

destructor TPenStylePropertyForSelect.Destroy;
begin
  FreeAndNil(PenStylesBox);
  FreeAndNil(PenStylesLabel);
end;

destructor TBrushStylePropertyForSelect.Destroy;
begin
  FreeAndNil(BrushStylesBox);
  FreeAndNil(BrushStylesLable);
end;

destructor TColorPropertyForSelect.Destroy;
begin
  FreeAndNil(Button);
end;

procedure TPenStylePropertyForSelect.PenStylesBoxDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  Y: integer;
begin
  Y := ARect.Top + 7;
  (Control as TComboBox).Canvas.Pen.Style := CasePenStyle(Index);
  (Control as TComboBox).Canvas.Line(0, Y, 200, Y);
end;

procedure TBrushStylePropertyForSelect.BrushStylesBoxDrawItem(Control: TWinControl;
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
