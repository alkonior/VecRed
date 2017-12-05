unit STools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin,
  FPCanvas, TypInfo, LCL, SFigures, UScale;

{ Classes }

type
  TProc = procedure(Sender: TObject);

  TMyButton = class
    Button: TSpeedButton;
    Proc: TProc;
    procedure OnClick(Sender: TObject);
    constructor Create(PR: TProc; Panel: TPanel; i,top,left: integer;s:String);
    destructor Destroy(); override;
  end;

  { TProperty }

  TProperty = class
  end;

  ArrayOfProperty = array of TProperty;

  TSpinProperty = class(TProperty)
    SpinLabel: TLabel;
    SpinEdit: TSpinEdit;
    constructor Create(s: string; n: integer);
    destructor Destroy(); override;
  end;

  TPenStyleProperty = class(TProperty)
    PenStylesLabel: TLabel;
    PenStylesBox: TComboBox;
    constructor Create(n: integer);
    destructor Destroy(); override;
    procedure PenStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStylesLable: TLabel;
    BrushStylesBox: TComboBox;
    constructor Create(n: integer);
    destructor Destroy(); override;
    procedure BrushStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  { TTool }

  TTool = class
    Icon: string;
    IsMainTool: boolean;
    procedure FigureCreate(Point: TFloatPoint); virtual; abstract;
    procedure ChangePoint(Point: TFloatPoint); virtual; abstract;
    procedure AddPoint(Point: TFloatPoint); virtual; abstract;
    procedure MouseUp(Point: TFloatPoint); virtual; abstract;
    procedure FigureEnd(); virtual; abstract;
    procedure CreateParams(); virtual; abstract;
    procedure DeleteParams(); virtual; abstract;
  end;

  { STools }

  TPolylineTool = class(TTool)
  public
    PRPWidth: TSpinProperty;
    PRPPenStyle: TPenStyleProperty;
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;


  TLineTool = class(TTool)
  public
    PRPWidth: TSpinProperty;
    PRPPenStyle: TPenStyleProperty;
    constructor Create;
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
    PRPWidth: TSpinProperty;
    PRPPenStyle: TPenStyleProperty;
    PRPBrushStyle: TBrushStyleProperty;
    constructor Create;
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
    PRPWidth, PRPRadY, PRPRadX: TSpinProperty;
    PRPPenStyle: TPenStyleProperty;
    PRPBrushStyle: TBrushStyleProperty;
    constructor Create;
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
    PRPWidth: TSpinProperty;
    PRPPenStyle: TPenStyleProperty;
    PRPBrushStyle: TBrushStyleProperty;
    constructor Create;
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
    constructor Create;
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
    constructor Create;
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
    Delete:TMyButton;
    AllTop:TMyButton;
    Allbottom:TMyButton;
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

  TScrollTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure MouseUp(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure CreateParams(); override;
    procedure DeleteParams(); override;
  end;

procedure Changetool(Sender: TObject);
procedure DeleteFigures(Sender: TObject);

var           { Var }
  Tools: array of TTool;
  AButtons: array of TMyButton;
  SPoint: TFloatPoint;
  ShiftButtonState: boolean = False;
  PropertyPanel: TPanel;
  ChoosenTool: TTool;

implementation

{ Porocedures }
procedure Changetool(Sender: TObject);
var i:TFigure;
begin
  if (ChoosenTool <> Tools[(Sender as TSpeedButton).tag]) then
  begin
    ChoosenTool.FigureEnd();
    for i in figures do
      i.Selected := False;
    InvalidateHandler;
    ChoosenTool.DeleteParams();
    ChoosenTool := Tools[(Sender as TSpeedButton).Tag];
    ChoosenTool.CreateParams();
  end;
end;
procedure DeleteFigures(Sender: TObject);
begin
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

procedure TPolylineTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TPolyLine.Create(point, PRPWidth.SpinEdit.Value,
    CasePenStyle(PRPPenStyle.PenStylesBox.ItemIndex));
  SetLength(Figures[High(Figures)].Points, 4);
  Figures[High(Figures)].Points[3] := Point;
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;


procedure TLineTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TLine.Create(point, PRPWidth.SpinEdit.Value,
    CasePenStyle(PRPPenStyle.PenStylesBox.ItemIndex));
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TEllipseTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(point, PRPWidth.SpinEdit.Value,
    CasePenStyle(PRPPenStyle.PenStylesBox.ItemIndex),
    CaseBrushStyle(PRPBrushStyle.BrushStylesBox.ItemIndex));
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TRectangleTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(point, PRPWidth.SpinEdit.Value,
    CasePenStyle(PRPPenStyle.PenStylesBox.ItemIndex),
    CaseBrushStyle(PRPBrushStyle.BrushStylesBox.ItemIndex));
  MinPoint := min(MinPoint, point);
  MaxPoint := max(MaxPoint, point);
end;

procedure TRoundRectTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRoundRect.Create(point, PRPWidth.SpinEdit.Value,
    PRPRadX.SpinEdit.Value, PRPRadY.SpinEdit.Value,
    CasePenStyle(PRPPenStyle.PenStylesBox.ItemIndex),
    CaseBrushStyle(PRPBrushStyle.BrushStylesBox.ItemIndex));
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
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectZoom.Create(point);
  for i in figures do
    i.Selected := False;
end;

procedure TScrollTool.FigureCreate(Point: TFloatPoint);
begin
  SPoint := Point;
end;


{ ChangePoint }

procedure TPolylineTool.ChangePoint(Point: TFloatPoint);
begin
  if ShiftButtonState then
  begin
    MinPoint := min(MinPoint, point);
    MaxPoint := max(MaxPoint, point);
    with Figures[high(Figures)] do
    begin
      SetLength(Points, Length(Points) + 1);
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

procedure TScrollTool.ChangePoint(Point: TFloatPoint);
begin
  if length(Figures) > 0 then
    Offset := Offset + WorldToScrn(spoint) - WorldToScrn(point);
end;

{ AddPoint }

procedure TPolylineTool.AddPoint(Point: TFloatPoint);
begin
  if not (ShiftButtonState) then
  begin
    MinPoint := min(MinPoint, point);
    MaxPoint := max(MaxPoint, point);
    with Figures[high(Figures)] as TPolyline do
    begin
      SetLength(points, Length(points) + 1);
      points[high(points)] := point;
      Points[0] := min(Point, Points[0]);
      Points[1] := max(Point, Points[1]);
    end;
  end;
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

procedure TPolylineTool.MouseUp(Point: TFloatPoint);
begin
  if ShiftButtonState then
    Drawing := False;
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
end;

procedure TEllipseTool.MouseUp(Point: TFloatPoint);
begin
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

procedure TPolylineTool.FigureEnd();
begin
  if Length(Figures)>0 then
  if Figures[High(Figures)] is TPolyline then
  with Figures[High(Figures)] as TPolyline do
  begin
    Points[0]:=min(Points[0],Points[High(Points)]);
    Points[1]:=max(Points[1],Points[High(Points)]);
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
  if Length(Figures)>0 then
  if Figures[high(Figures)] is TRectZoom then
  begin
    FreeAndNil(Figures[High(Figures)]);
    SetLength(Figures, Length(Figures) - 1);
  end;
  Drawing := False;
end;

procedure TSelectTool.FigureEnd();
begin
  if Length(Figures)>0 then
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

procedure TPolylineTool.CreateParams();
begin
  PRPWidth := TSpinProperty.Create('Width', 1);
  PRPPenStyle := TPenStyleProperty.Create(2);
  PropertyPanel.Height := 77;
end;

procedure TLineTool.CreateParams();
begin
  PRPWidth := TSpinProperty.Create('Width', 1);
  PRPPenStyle := TPenStyleProperty.Create(2);
  PropertyPanel.Height := 77;
end;

procedure TRectangleTool.CreateParams();
begin
  PRPWidth := TSpinProperty.Create('Width', 1);
  PRPPenStyle := TPenStyleProperty.Create(2);
  PRPBrushStyle := TBrushStyleProperty.Create(3);
  PropertyPanel.Height := 78 + 39 - 1;
end;

procedure TEllipseTool.CreateParams();
begin
  PRPWidth := TSpinProperty.Create('Width', 1);
  PRPPenStyle := TPenStyleProperty.Create(2);
  PRPBrushStyle := TBrushStyleProperty.Create(3);
  PropertyPanel.Height := 78 + 39 - 1;
end;

procedure TRoundRectTool.CreateParams();
begin
  PRPWidth := TSpinProperty.Create('Width', 1);
  PRPPenStyle := TPenStyleProperty.Create(2);
  PRPBrushStyle := TBrushStyleProperty.Create(3);
  PRPRadX := TSpinProperty.Create('Rad X', 4);
  PRPRadY := TSpinProperty.Create('Rad Y', 5);
  PropertyPanel.Height := 78 + 39 * 3 - 3;
end;

procedure TRectZoomTool.CreateParams();
begin
end;

procedure TSelectTool.CreateParams();
begin
  Delete:=TMyButton.Create((@DeleteFigures),PropertyPanel,0,0,5,'ico/delete.png');
  PropertyPanel.Height:=35;
end;

procedure TZoomTool.CreateParams();
begin
end;

procedure TScrollTool.CreateParams();
begin
end;

{ DeleteParams }

procedure TPolylineTool.DeleteParams();
begin
  PRPWidth.Destroy();
  PRPPenStyle.Destroy();
end;

procedure TLineTool.DeleteParams();
begin
  PRPWidth.Destroy();
  PRPPenStyle.Destroy();
end;

procedure TRectangleTool.DeleteParams();
begin
  PRPWidth.Destroy();
  PRPPenStyle.Destroy();
  PRPBrushStyle.Destroy();
end;

procedure TRoundRectTool.DeleteParams();
begin
  PRPWidth.Destroy();
  PRPPenStyle.Destroy();
  PRPRadX.Destroy();
  PRPRadY.Destroy();
  PRPBrushStyle.Destroy();
end;

procedure TRectZoomTool.DeleteParams();
begin
end;

procedure TSelectTool.DeleteParams();
begin
  Delete.Destroy();
end;

procedure TEllipseTool.DeleteParams();
begin
  PRPWidth.Destroy();
  PRPPenStyle.Destroy();
  PRPBrushStyle.Destroy();
end;

procedure TZoomTool.DeleteParams();
begin
end;

procedure TScrollTool.DeleteParams();
begin
end;

{ PRPCreateDestroy }
constructor TSpinProperty.Create(s: string; n: integer);
begin
  SpinLabel := TLabel.Create(PropertyPanel);
  SpinLabel.Caption := s;
  SpinLabel.Align := alTop;
  SpinLabel.Top := n * 100;
  SpinLabel.Parent := PropertyPanel;
  SpinEdit := TSpinEdit.Create(PropertyPanel);
  SpinEdit.Align := alTop;
  SpinEdit.Parent := PropertyPanel;
  SpinEdit.MinValue := 1;
  SpinEdit.MaxValue := 1000000;
  SpinEdit.Value := 1;
  SpinEdit.Top := n * 100 + 50;
  SpinEdit.Alignment := taLeftJustify;
end;

constructor TPenStyleProperty.Create(n: integer);
begin
  PenStylesLabel := TLabel.Create(PropertyPanel);
  PenStylesLabel.Top := 100 * n;
  PenStylesLabel.Caption := 'Pen Style';
  PenStyleslabel.Align := alTop;
  PenStylesLabel.Parent := PropertyPanel;
  PenStylesBox := TComboBox.Create(PropertyPanel);
  PenStylesBox.Align := alTop;
  PenStylesBox.ReadOnly := True;
  PenStylesBox.Top := 100 * n + 50;
  PenStylesBox.Items.CommaText := ',,,,';
  PenStylesBox.Style := csOwnerDrawFixed;
  PenStylesBox.ItemIndex := 0;
  PenStylesBox.Parent := PropertyPanel;
  PenStylesBox.OnDrawItem := @PenStylesBoxDrawItem;
end;

constructor TBrushStyleProperty.Create(n: integer);
begin
  BrushStylesLable := TLabel.Create(PropertyPanel);
  BrushStylesLable.Top := 100 * n;
  BrushStylesLable.Caption := 'Brush Style';
  BrushStylesLable.Align := alTop;
  BrushStylesLable.Parent := PropertyPanel;
  BrushStylesBox := TComboBox.Create(PropertyPanel);
  BrushStylesBox.Items.CommaText := ',,,,,,,';
  BrushStylesBox.ReadOnly := True;
  BrushStylesBox.Top := 100 * n + 50;
  BrushStylesBox.Parent := PropertyPanel;
  BrushStylesBox.Style := csOwnerDrawFixed;
  BrushStylesBox.ItemIndex := 0;
  BrushStylesBox.Align := alTop;
  BrushStylesBox.OnDrawItem := @BrushStylesBoxDrawItem;
end;

destructor TSpinProperty.Destroy();
begin
  inherited Destroy;
  FreeAndNil(SpinEdit);
  FreeAndNil(SpinLabel);
end;

destructor TPenStyleProperty.Destroy();
begin
  inherited Destroy;
  FreeAndNil(PenStylesBox);
  FreeAndNil(PenStylesLabel);
end;

destructor TBrushStyleProperty.Destroy();
begin
  inherited Destroy;
  FreeAndNil(BrushStylesBox);
  FreeAndNil(BrushStylesLable);
end;

{ ToolCreate }

constructor TPolylineTool.Create;
begin
  Icon := 'ico/polyline.png';
  IsMainTool := True;
end;

constructor TLineTool.Create;
begin
  Icon := 'ico/line.png';
  IsMainTool := True;
end;

constructor TRectangleTool.Create;
begin
  Icon := 'ico/rectangle.png';
  IsMainTool := True;
end;

constructor TEllipseTool.Create;
begin
  Icon := 'ico/ellipse.png';
  IsMainTool := True;
end;

constructor TZoomTool.Create;
begin
  Icon := 'ico/zoom.png';
  IsMainTool := True;
end;

constructor TScrollTool.Create;
begin
  Icon := 'ico/Scroll.png';
  IsMainTool := True;
end;

constructor TRectZoomTool.Create;
begin
  Icon := 'ico/rectzoom.png';
  IsMainTool := True;
end;

constructor TRoundRectTool.Create;
begin
  Icon := 'ico/RoundRectangle.png';
  IsMainTool := True;
end;

constructor TSelectTool.Create;
begin
  Icon := 'ico/select.png';
  IsMainTool := True;
end;

{ TMyButton }
constructor TMyButton.Create(PR: TProc; Panel: TPanel; i,top,left: integer;s:string);
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

initialization
  RegisterTool(TPolylineTool.Create);
  RegisterTool(TLineTool.Create);
  RegisterTool(TRectangleTool.Create);
  RegisterTool(TEllipseTool.Create);
  RegisterTool(TRoundRectTool.Create);
  RegisterTool(TZoomTool.Create);
  RegisterTool(TRectZoomTool.Create);
  RegisterTool(TScrollTool.Create);
  RegisterTool(TSelectTool.Create);
end.
