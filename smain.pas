unit SMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, LCLIntf, LCLType, Buttons, Math, spropertes,
  FPCanvas, TypInfo, Spin, SFigures, STools, Types, UScale, GraphMath;

type

  { TVecRedF }

  TVecRedF = class(TForm)
    CB1: TColorButton;
    CB2: TColorButton;
    CloseB: TMenuItem;
    ColorPanel: TPanel;
    MMenu: TMainMenu;
    Reset: TMenuItem;
    Options: TMenuItem;
    MenuItem3: TMenuItem;
    DeleteALL: TMenuItem;
    ScrollBarBottom: TScrollBar;
    ScrollBarRight: TScrollBar;
    Spravka: TMenuItem;
    ToolPanel: TPanel;
    MPanel: TPanel;
    PB: TPaintBox;
    PBPanel: TPanel;
    ZoomB: TSpinEdit;
    ZoomT: TLabel;
    procedure C1Change(Sender: TObject);
    procedure C2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DeleteALLClick(Sender: TObject);
    procedure CloseBClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PBMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure PBResize(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure SpravkaClick(Sender: TObject);
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PBPaint(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure CreateButton(i: integer);
    procedure ZoomBChange(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  VecRedF: TVecRedF;
  ChoosenTool: TTool;
  Mooving, ScrollB: boolean;
  cy, cx: integer;

implementation

{$R *.lfm}


{ TVecRedF }

procedure TVecRedF.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    VK_SHIFT:
    begin
      ShiftB := True;
    end;
  end;
end;

procedure TVecRedF.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    VK_SHIFT:
    begin
      ShiftB := False;
    end;
    VK_SPACE:
    begin
      ChoosenTool.FigureEnd();
    end;
  end;
end;

procedure TVecRedF.FormCreate(Sender: TObject);
var
  i: integer;
begin
  WindowWH := Point(PB.Width, PB.Height);
  WindowLWH := WindowWH;
  ScrollLWH := Round(WindowWH);
  ScrollBarBottom.Max := WindowWH.x;
  ScrollBarRight.max := WindowWH.y;
  MinPoint := FloatPoint(10000, 100000);
  MaxPoint := FloatPoint(-10000, -10000);
  Offset := FloatPoint(0, 0);
  for i := 0 to High(Tools) do
  begin
    CreateButton(i);
  end;
  MPanel.top := 10;
  MPanel.Left := 10;
  PropertyPanel.Parent := VecRedF;
  PropertyPanel.AnchorSide[akTop].Side := asrBottom;
  PropertyPanel.AnchorSide[akTop].Control := MPanel;
  PropertyPanel.AnchorSide[akLeft].Side := asrLeft;
  PropertyPanel.AnchorSide[akleft].Control := MPanel;
  PropertyPanel.BevelInner := bvNone;
  PropertyPanel.BevelOuter := bvNone;
  PropertyPanel.Width := MPanel.Width;
  PropertyPanel.BorderWidth := 0;
  PropertyPanel.OnMouseDown := @MPanelMouseDown;
  PropertyPanel.OnMouseMove := @MPanelMouseMove;
  PropertyPanel.OnMouseUp := @MPanelMouseUp;
  ChoosenTool := Tools[0];
  InvalidateHandler := @Invalidate;
  ChoosenTool.PropertiesCreate();
end;

procedure TVecRedF.DeleteALLClick(Sender: TObject);
begin
  Setlength(Figures, 0);
  MinPoint := FloatPoint(10000, 100000);
  MaxPoint := FloatPoint(-10000, -10000);
  Offset := FloatPoint(0, 0);
  PB.Invalidate;
end;

procedure TVecRedF.CloseBClick(Sender: TObject);
begin
  Close();
end;

procedure TVecRedF.MPanelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Mooving := True;
  cx := x;
  cy := y;
end;

procedure TVecRedF.MPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if Mooving then
  begin
    mpanel.top := mpanel.top + y - cy;
    MPanel.Left := MPanel.Left + x - cx;
    if (MPanel.Top < 0) or (MPanel.Top > VecRedF.Height - 70) then
      mpanel.top := mpanel.top - y + cy;
    if (MPanel.left < 0) or (MPanel.left > VecRedF.Width - 50) then
      mpanel.left := mpanel.left - x + cx;
  end;
end;

procedure TVecRedF.MPanelMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  Mooving := False;
end;

procedure TVecRedF.PBMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  point: TFloatPoint;
begin
  spoint.x := WindowWH.x div 2;
  spoint.y := WindowWH.y div 2;
  point := ScrnToWorld(spoint);
  if WheelDelta > 0 then
    zoom := round(zoom * 1.2) + 2
  else
    zoom := round(zoom / 1.2) + 2;
  if zoom > 1000 then
    zoom := 1000;
  if zoom < 0 then
    zoom := 1;
  Offset := Point * (zoom / 100) - spoint;
  spoint := ScrnToWorld(spoint);
  ZoomB.Value := zoom;
  PB.Invalidate;
end;

procedure TVecRedF.PBResize(Sender: TObject);
begin
  WindowWH := Point(PB.Width, PB.Height);
  ChangeCenter();
  WindowLWH := WindowWH;
end;

procedure TVecRedF.ResetClick(Sender: TObject);
begin
  if (minpoint.x < maxpoint.x) and (minpoint.y < maxpoint.y) then
    ZoomToRect((minpoint - FloatPoint(50, 50)), (maxpoint + FloatPoint(50, 50)));
  ZoomB.Value := zoom;
end;

procedure TVecRedF.ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: integer);
begin
  if not ScrollB then
  begin
    Offset := point(ScrollBarBottom.Position, ScrollBarRight.Position);
    Invalidate;
  end;
  ScrollB := False;
end;

procedure TVecRedF.SpravkaClick(Sender: TObject);
begin
  ShowMessage('Это векторный редактор. Он рисует. Александр Дзюба');
end;

procedure TVecRedF.ToolClick(Sender: TObject);
begin
  if Drawing then
  begin
    FreeAndNil(Figures[High(Figures)]);
    SetLength(Figures, Length(Figures) - 1);
    Drawing := False;
    PB.Invalidate;
    MaxPoint := LLMaxPoint;
    MinPoint := LLMinPoint;
  end;
  ChoosenTool := Tools[(Sender as TSpeedButton).Tag];
  DeletePRP();
  ChoosenTool.PropertiesCreate();
end;

procedure TVecRedF.PBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Drawing then
  begin
    ChoosenTool.AddPoint(ScrnToWorld(point(x, y)));
    PB.Invalidate;
  end
  else
  begin
    ChoosenTool.FigureCreate(ScrnToWorld(point(x, y)));
    Invalidate;
    Drawing := True;
  end;
end;

procedure TVecRedF.PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if Drawing then
  begin
    ChoosenTool.ChangePoint(ScrnToWorld(point(x, y)));
  end;
  Invalidate;
end;

procedure TVecRedF.PBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ChoosenTool.MouseUp(ScrnToWorld(Point(x,y)));
  ZoomB.Value := zoom;
  Invalidate;
end;

procedure TVecRedF.PBPaint(Sender: TObject);
var
  i: TFigure;
begin
  SetScrolBars(ScrollBarBottom, ScrollBarRight);
  ScrollB := True;
  for i in Figures do
    i.draw(pb.Canvas);
end;

procedure TVecRedF.C1Change(Sender: TObject);
begin
  if Drawing then
  begin
    Figures[high(Figures)].C1 := CB1.ButtonColor;
    PB.Invalidate;
  end;
  PenColor := CB1.ButtonColor;
end;

procedure TVecRedF.C2Change(Sender: TObject);
begin
  if Drawing then
  begin
    Figures[high(Figures)].C2 := CB2.ButtonColor;
    PB.Invalidate;
  end;
  BrushColor := CB2.ButtonColor;
end;

procedure TVecRedF.CreateButton(i: integer);
var
  ToolBtn: TSpeedButton;
  ToolIcon: TBitmap;
begin
  ToolBtn := TSpeedButton.Create(VecRedF);
  ToolIcon := TBitmap.Create;
  with TPicture.Create do
  begin
    LoadFromFile(Tools[i].Icon);
    ToolIcon.Assign(Graphic);
  end;
  ToolBtn.Transparent := True;
  ToolIcon.Transparent := True;
  ToolBtn.Glyph := ToolIcon;
  ToolBtn.Width := 32;
  ToolBtn.Height := 32;
  ToolBtn.Top := (i div 3) * 32;
  ToolBtn.Left := (i mod 3) * 32;
  ToolBtn.Tag := i;
  ToolBtn.GroupIndex := 1;
  ToolBtn.Down := i = 0;
  ToolBtn.OnClick := @ToolClick;
  ToolBtn.Parent := ToolPanel;
end;

procedure TVecRedF.ZoomBChange(Sender: TObject);
var
  oldz: integer;
begin
  oldz := zoom;
  Zoom := ZoomB.Value;
  CenterZoom(oldz);
  PB.Invalidate;
end;



end.
