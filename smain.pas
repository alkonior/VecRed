unit SMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, LCLIntf, LCLType, Buttons, Math,
  FPCanvas, TypInfo, Spin, SFigures, STools, Types, UScale, GraphMath,
  Laz2_DOM, laz2_XMLRead, laz2_XMLWrite, SynHighlighterXML, LCLProc, Clipbrd,
  SHistroy, SParams;

const
  Name = 'VecRed';

type

  { TVecRedF }

  TVecRedF = class(TForm)
    CloseB: TMenuItem;
    ColorPanel: TPanel;
    Test: TMenuItem;
    Open: TMenuItem;
    OpenDialog: TOpenDialog;
    Save: TMenuItem;
    SaveAs: TMenuItem;
    MMenu: TMainMenu;
    CustomPanel: TPanel;
    Reset: TMenuItem;
    Options: TMenuItem;
    MenuItem3: TMenuItem;
    DeleteALL: TMenuItem;
    SaveDialog: TSaveDialog;
    ScrollBarBottom: TScrollBar;
    ScrollBarRight: TScrollBar;
    Spravka: TMenuItem;
    ToolPanel: TPanel;
    MPanel: TPanel;
    PB: TPaintBox;
    PBPanel: TPanel;
    ZoomB: TSpinEdit;
    ZoomT: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure DeleteALLClick(Sender: TObject);
    procedure CloseBClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MMenuChange(Sender: TObject; Source: TMenuItem; Rebuild: boolean);
    procedure MPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure MPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure MPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure OpenClick(Sender: TObject);
    procedure PBDblClick(Sender: TObject);
    procedure PBMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure PBResize(Sender: TObject);
    procedure ResetClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure ScrollScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: integer);
    procedure SpravkaClick(Sender: TObject);
    procedure PBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PBMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PBPaint(Sender: TObject);
    procedure TestClick(Sender: TObject);
    procedure ZoomBChange(Sender: TObject);
    function IsSaveDialog(): integer;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  VecRedF: TVecRedF;
  Mooving, ScrollB: boolean;
  cy, cx: integer;
  FileName: string;

implementation

{$R *.lfm}

{ TVecRedF }

function TVecRedF.IsSaveDialog(): integer;
begin
  Result := MessageDlg('Save changes?', 'File has been modified, save changes?',
    mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

procedure TVecRedF.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    VK_SHIFT: ShiftButtonState := True;
    VK_DELETE: DeleteFigures(Sender);
    VK_CONTROL: CtrlButtonState := True;
    VK_S: if (ShiftButtonState) and (CtrlButtonState) then
        SaveAs.Click
      else if ShiftButtonState then
        Save.Click;
    VK_Z: if CtrlButtonState then
        if ShiftButtonState then
        begin
          if Current < length(History)-1 then
          begin
            Inc(Current);
            History[Current].LoadFigures();
            Drawing:=false;
            Invalidate;
          end;
        end
        else
        begin
          if Current > 0 then
          begin
            Current := Current - 1;
            History[Current].LoadFigures();
            Drawing:=false;
            Invalidate;
          end;
        end;
  end;
end;


procedure TVecRedF.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    VK_SHIFT: ShiftButtonState := False;
    VK_SPACE: ChoosenTool.FigureEnd();
    VK_CONTROL: CtrlButtonState := False;
  end;
end;

procedure TVecRedF.MMenuChange(Sender: TObject; Source: TMenuItem; Rebuild: boolean);
begin

end;

procedure TVecRedF.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ColorPanelTool := ColorPanel;
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
    if Tools[i].IsMainTool then
    begin
      SetLength(AButtons, Length(AButtons) + 1);
      AButtons[High(AButtons)] :=
        TMyButton.Create((@ChangeMainTool), ToolPanel, i,
        ((Length(AButtons) - 1) div 4) * 33, ((Length(AButtons) - 1) mod 4) *
        33, Tools[i].Icon);
    end;
  end;
  MPanel.top := 10;
  MPanel.Left := 10;
  ButtonPanel := TPanel.Create(VecRedF);
  ButtonPanel.Parent := CustomPanel;
  ButtonPanel.AnchorSide[akTop].Side := asrTop;
  ButtonPanel.AnchorSide[akTop].Control := CustomPanel;
  ButtonPanel.AnchorSide[akLeft].Side := asrLeft;
  ButtonPanel.AnchorSide[akleft].Control := MPanel;
  ButtonPanel.BevelInner := bvNone;
  ButtonPanel.BevelOuter := bvNone;
  ButtonPanel.BorderWidth := 0;
  PropertyPanel := TPanel.Create(VecRedF);
  PropertyPanel.Parent := CustomPanel;
  PropertyPanel.AnchorSide[akTop].Side := asrBottom;
  PropertyPanel.AnchorSide[akTop].Control := ButtonPanel;
  PropertyPanel.AnchorSide[akLeft].Side := asrLeft;
  PropertyPanel.AnchorSide[akleft].Control := ButtonPanel;
  PropertyPanel.BevelInner := bvNone;
  PropertyPanel.BevelOuter := bvNone;
  PropertyPanel.BorderWidth := 0;
  PropertyPanel.OnMouseDown := @MPanelMouseDown;
  PropertyPanel.OnMouseMove := @MPanelMouseMove;
  PropertyPanel.OnMouseUp := @MPanelMouseUp;
  ButtonPanel.Color := MPanel.Color;
  ChoosenTool := Tools[0];
  InvalidateHandler := @Invalidate;
  ChoosenTool.CreateParams();
  SetLength(History, 1);
  History[0] := THistoryBlock.Create(FiguresToString());
  Current := 0;
  Saved := 0;
end;

procedure TVecRedF.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Ans: integer;
begin
  if Current = Saved then
    Exit;
  Ans := IsSaveDialog;
  if Ans = mrYes then
    Save.Click
  else if Ans = mrNo then
    CanClose := True
  else
    CanClose := False;
  if CanClose then
  begin
    for ans := 0 to high(History) do
      History[ans].Free;
  end;
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


procedure TVecRedF.PBDblClick(Sender: TObject);
begin
  ChoosenTool.FigureEnd();
end;

procedure TVecRedF.PBMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  point, spoint: TFloatPoint;
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

procedure TVecRedF.SaveAsClick(Sender: TObject);

begin
  if SaveDialog.Execute then
  begin
    TFigure.SaveFile(SaveDialog.FileName);
    FileName := SaveDialog.FileName;
    Saved := Current;
  end;
end;

procedure TVecRedF.SaveClick(Sender: TObject);
begin
  if FileName = '' then
    SaveAs.Click
  else
  begin
    TFigure.SaveFile(FileName);
    Saved := Current;
  end;
end;

procedure TVecRedF.OpenClick(Sender: TObject);
var
  i: TFigure;
  ans: integer;
  f: TFigure;
begin
  if Saved <> Current then
  begin
    Ans := IsSaveDialog();
    if Ans = mrYes then
      Save.Click
    else
    if Ans = mrCancel then
      Exit;
  end;
  if (OpenDialog.Execute) then
    if (TFigure.LoadFile(OpenDialog.FileName)) then
    begin
      FileName := OpenDialog.FileName;
      VecRedF.Caption := OpenDialog.FileName + ' - ' + Name;
      MinPoint := FloatPoint(100000, 100000);
      MaxPoint := FloatPoint(-100000, -100000);
      for i in Figures do
      begin
        MinPoint := Min(min(i.Points[0], i.Points[1]), MinPoint);
        MaxPoint := Max(max(i.Points[0], i.Points[1]), MaxPoint);
      end;
      SetLength(History, 1);
      History[0] := THistoryBlock.Create(FiguresToString());
      Current := 0;
      Saved := 0;
      Invalidate;
    end
    else
    begin
      ShowMessage('Файл поврежден!');
      setlength(Figures, 0);
    end;

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
  ChoosenTool.MouseUp(ScrnToWorld(Point(x, y)));
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
    if i <> nil then
      i.draw(pb.Canvas);
  for i in Figures do
    if i <> nil then
      if i.Selected then
        i.drawoutline(pb.Canvas);
end;

procedure TVecRedF.TestClick(Sender: TObject);
var
  S: string;
begin
  try
    Clipboard.AsText := FiguresToString();
  finally
  end;
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
