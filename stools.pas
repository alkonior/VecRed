unit STools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin,
  FPCanvas, TypInfo, LCL, SFigures, UScale;

{ Classes }

type
  { TProperty }
  TProperty = class
  end;

  ArrayOfProperty = array of TProperty;

  TSpinProperty = class(TProperty)
    SpinLabel: TLabel;
    SpinEdit: TSpinEdit;
    constructor Create(s: string; n: integer);
    destructor Destroy();
  end;

  TPenStyleProperty = class(TProperty)
    PenStylesLabel: TLabel;
    PenStylesBox: TComboBox;
    constructor Create(n: integer);
    destructor Destroy();
    procedure PenStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStylesLable: TLabel;
    BrushStylesBox: TComboBox;
    constructor Create(n: integer);
    destructor Destroy();
    procedure BrushStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  { TTool }
  TTool = class
    Icon: string;
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

  TPenTool = class(TTool)
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

var           { Var }
  Tools: array of TTool;
  SPoint: TFloatPoint;
  ShiftB: boolean = False;
  PropertyPanel: TPanel;

implementation

{ Porocedures }

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
  {GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TPolyLine.Create(point);
  Figures[High(Figures)].SetParams();  }
end;

procedure TPenTool.FigureCreate(Point: TFloatPoint);
begin
  {GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TPolyLine.Create(point);
  Figures[High(Figures)].SetParams();}
end;

procedure TLineTool.FigureCreate(Point: TFloatPoint);
begin
 { GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TLine.Create(point);
  Figures[High(Figures)].SetParams();     }
end;

procedure TEllipseTool.FigureCreate(Point: TFloatPoint);
begin
{  GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(point);
  Figures[High(Figures)].SetParams();   }
end;

procedure TRectangleTool.FigureCreate(Point: TFloatPoint);
begin
 { GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(point);
  Figures[High(Figures)].SetParams();    }
end;

procedure TRoundRectTool.FigureCreate(Point: TFloatPoint);
begin
  {GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRoundRect.Create(point);
  Figures[High(Figures)].SetParams();  }
end;

procedure TZoomTool.FigureCreate(Point: TFloatPoint);
begin
  spoint := WorldToScrn(point);
  if not (ShiftB) then
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
 { Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectZoom.Create(point);    }
end;

procedure TSelectTool.FigureCreate(Point: TFloatPoint);
var
  i: TFigure;
begin
  {Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectZoom.Create(point);
  for i in figures do
    i.Selected := False;  }
end;

procedure TScrollTool.FigureCreate(Point: TFloatPoint);
begin
  SPoint := Point;
end;


{ ChangePoint }

procedure TPolylineTool.ChangePoint(Point: TFloatPoint);
begin
 { MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;         }
end;

procedure TPenTool.ChangePoint(Point: TFloatPoint);
begin
  {MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  LMinPoint := MinPoint;
  LMaxPoint := MaxPoint;
  with Figures[high(Figures)] as TPolyline do
  begin
    SetLength(points, Length(Points) + 1);
    points[high(points)] := point;
    minp := min(minp, point);
    maxp := max(maxp, point);
  end;}
end;

procedure TLineTool.ChangePoint(Point: TFloatPoint);
begin
  {with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point); }
end;

procedure TRectangleTool.ChangePoint(Point: TFloatPoint);
begin
  {MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;   }
end;

procedure TRoundRectTool.ChangePoint(Point: TFloatPoint);
begin
 { MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;    }
end;

procedure TEllipseTool.ChangePoint(Point: TFloatPoint);
begin
 { MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;   }
end;

procedure TZoomTool.ChangePoint(Point: TFloatPoint);
begin
  {Offset := Offset + WorldToScrn(spoint) - WorldToScrn(point);   }
end;

procedure TRectZoomTool.ChangePoint(Point: TFloatPoint);
begin
  {with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;  }
end;

procedure TSelectTool.ChangePoint(Point: TFloatPoint);
begin
  {with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;    }
end;

procedure TScrollTool.ChangePoint(Point: TFloatPoint);
begin
  {if length(Figures) > 0 then
    Offset := Offset + WorldToScrn(spoint) - WorldToScrn(point);  }
end;

{ AddPoint }

procedure TPolylineTool.AddPoint(Point: TFloatPoint);
begin
  {MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  LMinPoint := MinPoint;
  LMaxPoint := MaxPoint;
  with Figures[high(Figures)] as TPolyline do
  begin
    SetLength(points, Length(points) + 1);
    points[high(points)] := point;
    minp := min(minp, point);
    maxp := max(maxp, point);
  end;                        }
end;

procedure TLineTool.AddPoint(Point: TFloatPoint);
begin
 { Drawing := False;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);   }
end;

procedure TRectangleTool.AddPoint(Point: TFloatPoint);
begin
  {Drawing := False;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);   }
end;

procedure TRoundRectTool.AddPoint(Point: TFloatPoint);
begin
  {Drawing := False;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point); }
end;

procedure TRectZoomTool.AddPoint(Point: TFloatPoint);
begin
end;

procedure TSelectTool.AddPoint(Point: TFloatPoint);
begin
end;

procedure TEllipseTool.AddPoint(Point: TFloatPoint);
begin
  Drawing := False;
end;

procedure TZoomTool.AddPoint(Point: TFloatPoint);
begin
end;

procedure TPenTool.AddPoint(Point: TFloatPoint);
begin
end;

procedure TScrollTool.AddPoint(Point: TFloatPoint);
begin
end;

{ MouseUp }

procedure TPolylineTool.MouseUp(Point: TFloatPoint);
begin
end;

procedure TPenTool.MouseUp(Point: TFloatPoint);
begin
  { Drawing := False;   }
end;

procedure TLineTool.MouseUp(Point: TFloatPoint);
begin
end;

procedure TRectangleTool.MouseUp(Point: TFloatPoint);
begin
end;

procedure TRoundRectTool.MouseUp(Point: TFloatPoint);
begin
end;

procedure TRectZoomTool.MouseUp(Point: TFloatPoint);
begin
  {if Figures[high(Figures)] is TRectZoom then
  begin
    ZoomToRect(Figures[High(Figures)].Points[0], Figures[High(Figures)].Points[1]);
    FreeAndNil(Figures[High(Figures)]);
    SetLength(Figures, Length(Figures) - 1);
  end;
  Drawing := False;    }
end;

procedure TSelectTool.MouseUp(Point: TFloatPoint);
var
  i: integer;
begin
  {for i := 0 to Length(Figures) - 1 do
  begin
    Figures[i].Selected := False;
  end;
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
  Drawing := False;  }
end;

procedure TEllipseTool.MouseUp(Point: TFloatPoint);
begin
end;

procedure TZoomTool.MouseUp(Point: TFloatPoint);
begin
  {Drawing := False;  }
end;

procedure TScrollTool.MouseUp(Point: TFloatPoint);
begin
  { Drawing := False; }
end;


{ FigureEnd }
procedure TPenTool.FigureEnd();
begin
  Drawing := False;
end;

procedure TPolylineTool.FigureEnd();
begin
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
  {if Figures[high(Figures)] is TRectZoom then
  begin
    FreeAndNil(Figures[High(Figures)]);
    SetLength(Figures, Length(Figures) - 1);
  end;            }
  Drawing := False;
end;

procedure TSelectTool.FigureEnd();
begin
  {if Figures[high(Figures)] is TRectZoom then
  begin
    FreeAndNil(Figures[High(Figures)]);
    SetLength(Figures, Length(Figures) - 1);
  end;          }
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
end;

procedure TPenTool.CreateParams();
begin
  PRPWidth := TSpinProperty.Create('Width', 1);
  PRPPenStyle := TPenStyleProperty.Create(2);
end;

procedure TLineTool.CreateParams();
begin
end;

procedure TRectangleTool.CreateParams();
begin
end;

procedure TRoundRectTool.CreateParams();
begin
end;

procedure TRectZoomTool.CreateParams();
begin
end;

procedure TSelectTool.CreateParams();
begin
end;

procedure TEllipseTool.CreateParams();
begin
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

procedure TPenTool.DeleteParams();
begin
  PRPWidth.Destroy();
  PRPPenStyle.Destroy();
end;

procedure TLineTool.DeleteParams();
begin

end;

procedure TRectangleTool.DeleteParams();
begin

end;

procedure TRoundRectTool.DeleteParams();
begin

end;

procedure TRectZoomTool.DeleteParams();
begin

end;

procedure TSelectTool.DeleteParams();
begin

end;

procedure TEllipseTool.DeleteParams();
begin

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
  BrushStylesBox := TComboBox.Create(PropertyPanel);
  BrushStylesBox.Items.CommaText := ',,,,,,,';
  BrushStylesBox.ReadOnly := True;
  BrushStylesBox.Top := 100 * n + 50;
  BrushStylesBox.Style := csOwnerDrawFixed;
  BrushStylesBox.ItemIndex := 0;
  BrushStylesBox.Align := alTop;
  BrushStylesBox.OnDrawItem := @BrushStylesBoxDrawItem;
end;

destructor TSpinProperty.Destroy();
begin
  FreeAndNil(SpinEdit);
  FreeAndNil(SpinLabel);
end;

destructor TPenStyleProperty.Destroy();
begin
  FreeAndNil(PenStylesBox);
  FreeAndNil(PenStylesLabel);
end;

destructor TBrushStyleProperty.Destroy();
begin
  FreeAndNil(BrushStylesBox);
  FreeAndNil(BrushStylesLable);
end;

{ ToolCreate }

constructor TPolylineTool.Create;
begin
  Icon := 'ico/polyline.png';
end;

constructor TPenTool.Create;
begin
  Icon := 'ico/polyline.png';
end;

constructor TLineTool.Create;
begin
  Icon := 'ico/line.png';
end;

constructor TRectangleTool.Create;
begin
  Icon := 'ico/rectangle.png';
end;

constructor TEllipseTool.Create;
begin
  Icon := 'ico/ellipse.png';
end;

constructor TZoomTool.Create;
begin
  Icon := 'ico/zoom.png';
end;

constructor TScrollTool.Create;
begin
  Icon := 'ico/Scroll.png';
end;

constructor TRectZoomTool.Create;
begin
  Icon := 'ico/rectzoom.png';
end;

constructor TRoundRectTool.Create;
begin
  Icon := 'ico/RoundRectangle.png';
end;

constructor TSelectTool.Create;
begin
  Icon := 'ico/select.png';
end;

initialization
  RegisterTool(TPenTool.Create);
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
