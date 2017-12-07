unit SFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType,
  Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows, UScale;

type
  { Classes }
  { TFigure }

  TFigure = class
    Points: array of TFloatPoint;
    Selected: boolean;
    procedure move(point: TFloatPoint); virtual; abstract;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure DrawoutLine(Canvas: TCanvas); virtual; abstract;
    function PointInFigure(point: TFloatPoint): boolean; virtual; abstract;
    function CheckPoint(point: TFloatPoint): PFloatPoint; virtual; abstract;
    function FigureInrect(point1, point2: TFloatPoint): boolean; virtual; abstract;
  end;

  TPolyline = class(TFigure)
  private
    PC: TColor;
    W: integer;
    PS: TPenStyle;
    procedure SetW(i:integer);
  public
    property PenColor: TColor read PC write PC default clBlack;
    property Width: integer read W write SetW default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    constructor Create(point: TFloatPoint; Wd: integer; PStyle: TPenStyle);
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
  end;

  TLine = class(TFigure)
  private
    PC: TColor;
    W: integer;
    PS: TPenStyle;
    procedure SetW(i:integer);
  public
    property PenColor: TColor read PC write PC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    constructor Create(point: TFloatPoint; Wd: integer; PStyle: TPenStyle);
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
  end;

  TRectangle = class(TFigure)
  private
    PC: TColor;
    BC: TColor;
    W: integer;
    PS: TPenStyle;
    BS: TBrushStyle;
  public
    property PenColor: TColor read PC write PC default clBlack;
    property BrushColor: TColor read BC write BC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    property BrushStyle: TBrushStyle read BS write BS;
    constructor Create(point: TFloatPoint; Wd: integer; PStyle: TPenStyle;
      BStyle: TBrushStyle);
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
  end;

  TEllipse = class(TFigure)
  private
    PC: TColor;
    W: integer;
    PS: TPenStyle;
    BS: TBrushStyle;
    BC: TColor;
  public
    property BrushColor: TColor read BC write BC default clBlack;
    property PenColor: TColor read PC write PC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    property BrushStyle: TBrushStyle read BS write BS;
    constructor Create(point: TFloatPoint; Wd: integer; PStyle: TPenStyle;
      BStyle: TBrushStyle);
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
  end;

  TRectZoom = class(TFigure)
    constructor Create(point: TFloatPoint);
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
  end;

  TRoundRect = class(TFigure)
  private
    PC: TColor;
    W: integer;
    PS: TPenStyle;
    BS: TBrushStyle;
    RX: integer;
    RY: integer;
    BC: TColor;
  public
    property BrushColor: TColor read BC write BC default clBlack;
    property PenColor: TColor read PC write PC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    property BrushStyle: TBrushStyle read BS write BS;
    property RadiusX: integer read RX write RX;
    property RadiusY: integer read RY write RY;

    constructor Create(point: TFloatPoint; Wd, RadX, RadY: integer;
      PStyle: TPenStyle; BStyle: TBrushStyle);
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
  end;

var      { Var }
  Figures: array of TFigure;
  Drawing: boolean = False;
  SelectedNumber: integer = 0;
  ColorPen: TColor = clBlack;
  ColorBrush: TColor = clWhite;
  ShowPoits: boolean = False;

implementation

{ Porocedures }
{ Drow }

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(points[0]).x -
    WorldToScrn(points[1]).x) div 2 + 1, abs(WorldToScrn(points[0]).y -
    WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.Pen.Color := PC;
  Canvas.Brush.Color := BC;
  Canvas.Pen.Style := PS;
  Canvas.Brush.Style := BS;
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x + (Canvas.Pen.Width div 2),
    WorldToScrn(min(points[0], points[1])).y + (Canvas.Pen.Width div 2),
    WorldToScrn(max(points[0], points[1])).x - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).y - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    round((RY - (Canvas.Pen.Width div 2)) * zoom / 100),
    round((RX - (Canvas.Pen.Width div 2)) * zoom / 100));
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(points[0]).x -
    WorldToScrn(points[1]).x) div 2 + 1, abs(WorldToScrn(points[0]).y -
    WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.Pen.Color := PC;
  Canvas.Brush.Color := BC;
  Canvas.Pen.Style := PS;
  Canvas.Brush.Style := BS;
  Canvas.Rectangle(WorldToScrn(min(points[0], points[1])).x + (Canvas.Pen.Width div 2),
    WorldToScrn(min(points[0], points[1])).y + (Canvas.Pen.Width div 2),
    WorldToScrn(max(points[0], points[1])).x - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).y - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)));
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(points[0]).x -
    WorldToScrn(points[1]).x) div 2 + 1, abs(WorldToScrn(points[0]).y -
    WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.Pen.Color := PC;
  Canvas.Brush.Color := BC;
  Canvas.Pen.Style := PS;
  Canvas.Brush.Style := BS;
  Canvas.Ellipse(WorldToScrn(min(points[0], points[1])).x +
    (Canvas.Pen.Width div 2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(min(points[0], points[1])).y + (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).x - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).y - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)));
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  i: integer;
begin
  Canvas.Pen.Color := PC;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := PS;
  for i := 2 to length(points) - 2 do
  begin
    Canvas.Line(WorldToScrn(Points[i]), WorldToScrn(points[i + 1]));
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := PC;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := PS;
  Canvas.Line(WorldToScrn(Points[0]), WorldToScrn(points[1]));
end;

procedure TRectZoom.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psDashDot;
  Canvas.Rectangle(WorldToScrn(points[0]).x, WorldToScrn(points[0]).y,
    WorldToScrn(points[1]).x, WorldToScrn(points[1]).y);
  Canvas.Brush.Style := bsSolid;
end;

{ move }

procedure TRoundRect.move(point: TFloatPoint);
begin
  points[0] := Points[0] + point;
  points[1] := Points[1] + point;
end;

procedure TRectangle.move(point: TFloatPoint);
begin
  points[0] := Points[0] + point;
  points[1] := Points[1] + point;
end;

procedure TEllipse.move(point: TFloatPoint);
begin
  points[0] := Points[0] + point;
  points[1] := Points[1] + point;
end;

procedure TPolyline.move(point: TFloatPoint);
var
  i: integer;
begin
  points[0] := FloatPoint(100000000, 10000000);
  points[1] := FloatPoint(-100000000, -10000000);
  for i := 2 to Length(Points) - 1 do
  begin
    Points[i] := Points[i] + point;
    points[0] := min(Points[0], Points[i]);
    points[1] := max(Points[1], Points[i]);
  end;
end;

procedure TLine.move(point: TFloatPoint);
begin
  points[0] := Points[0] + point;
  points[1] := Points[1] + point;
end;

procedure TRectZoom.move(point: TFloatPoint);
begin
end;

{ DrawOutLine }

procedure TRoundRect.DrawOutLine(Canvas: TCanvas);
var
  Wid: integer;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  wid := min(W, min(abs(WorldToScrn(points[0]).x - WorldToScrn(points[1]).x) div
    2 + 1, abs(WorldToScrn(points[0]).y - WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x - 1,
    WorldToScrn(min(points[0], points[1])).y - 1,
    WorldToScrn(max(points[0], points[1])).x + 1,
    WorldToScrn(max(points[0], points[1])).y + 1,
    round((RY + (Wid)) * zoom / 100),
    round((RX + (Wid)) * zoom / 100));
  Canvas.Pen.Color := clWhite;
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2,
    round((RY + Wid) * zoom / 100),
    round((RX + Wid) * zoom / 100));
  Canvas.Pen.Color := (clWhite xor clRed);
  if ShowPoits then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Ellipse(WorldToScrn(min(Points[0], Points[1])).x - 5,
      WorldToScrn(min(Points[0], Points[1])).y - 5,
      WorldToScrn(min(Points[0], Points[1])).x + 5,
      WorldToScrn(min(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(max(Points[0], Points[1])).x - 5,
      WorldToScrn(max(Points[0], Points[1])).y - 5,
      WorldToScrn(max(Points[0], Points[1])).x + 5,
      WorldToScrn(max(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(min(Points[0], Points[1])).x - 5,
      WorldToScrn(max(Points[0], Points[1])).y - 5,
      WorldToScrn(min(Points[0], Points[1])).x + 5,
      WorldToScrn(max(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(max(Points[0], Points[1])).x - 5,
      WorldToScrn(min(Points[0], Points[1])).y - 5,
      WorldToScrn(max(Points[0], Points[1])).x + 5,
      WorldToScrn(min(Points[0], Points[1])).y + 5);
  end;
  Canvas.Pen.Mode := pmCopy;
end;

procedure TRectangle.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.Rectangle(WorldToScrn(min(points[0], points[1])).x - 1,
    WorldToScrn(min(points[0], points[1])).y - 1,
    WorldToScrn(max(points[0], points[1])).x + 1,
    WorldToScrn(max(points[0], points[1])).y + 1);
  Canvas.Pen.Color := clWhite;
  Canvas.Rectangle(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2);
  Canvas.Pen.Color := (clWhite xor clRed);
  if ShowPoits then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Ellipse(WorldToScrn(min(Points[0], Points[1])).x - 5,
      WorldToScrn(min(Points[0], Points[1])).y - 5,
      WorldToScrn(min(Points[0], Points[1])).x + 5,
      WorldToScrn(min(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(max(Points[0], Points[1])).x - 5,
      WorldToScrn(max(Points[0], Points[1])).y - 5,
      WorldToScrn(max(Points[0], Points[1])).x + 5,
      WorldToScrn(max(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(min(Points[0], Points[1])).x - 5,
      WorldToScrn(max(Points[0], Points[1])).y - 5,
      WorldToScrn(min(Points[0], Points[1])).x + 5,
      WorldToScrn(max(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(max(Points[0], Points[1])).x - 5,
      WorldToScrn(min(Points[0], Points[1])).y - 5,
      WorldToScrn(max(Points[0], Points[1])).x + 5,
      WorldToScrn(min(Points[0], Points[1])).y + 5);
  end;
  Canvas.Pen.Mode := pmCopy;
end;

procedure TEllipse.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.Ellipse(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2);
  Canvas.Pen.Color := clWhite;
  Canvas.Ellipse(WorldToScrn(min(points[0], points[1])).x - 4,
    WorldToScrn(min(points[0], points[1])).y - 4,
    WorldToScrn(max(points[0], points[1])).x + 4,
    WorldToScrn(max(points[0], points[1])).y + 4);
  Canvas.Pen.Color := (clWhite xor clRed);
  if ShowPoits then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Ellipse(WorldToScrn(min(Points[0], Points[1])).x - 5,
      WorldToScrn(min(Points[0], Points[1])).y - 5,
      WorldToScrn(min(Points[0], Points[1])).x + 5,
      WorldToScrn(min(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(max(Points[0], Points[1])).x - 5,
      WorldToScrn(max(Points[0], Points[1])).y - 5,
      WorldToScrn(max(Points[0], Points[1])).x + 5,
      WorldToScrn(max(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(min(Points[0], Points[1])).x - 5,
      WorldToScrn(max(Points[0], Points[1])).y - 5,
      WorldToScrn(min(Points[0], Points[1])).x + 5,
      WorldToScrn(max(Points[0], Points[1])).y + 5);
    Canvas.Ellipse(WorldToScrn(max(Points[0], Points[1])).x - 5,
      WorldToScrn(min(Points[0], Points[1])).y - 5,
      WorldToScrn(max(Points[0], Points[1])).x + 5,
      WorldToScrn(min(Points[0], Points[1])).y + 5);
  end;
  Canvas.Pen.Mode := pmCopy;
end;

procedure TPolyline.DrawOutLine(Canvas: TCanvas);
var
  i: integer;
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 2 - min(W, 50),
    WorldToScrn(min(points[0], points[1])).y - 2 - min(W, 50),
    WorldToScrn(max(points[0], points[1])).x + 2 + min(W, 50),
    WorldToScrn(max(points[0], points[1])).y + 2 + min(W, 50));
  Canvas.Pen.Color := clWhite;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 3 - min(W, 50),
    WorldToScrn(min(points[0], points[1])).y - 3 - min(W, 50),
    WorldToScrn(max(points[0], points[1])).x + 3 + min(W, 50),
    WorldToScrn(max(points[0], points[1])).y + 3 + min(W, 50));
  Canvas.Pen.Color := (clWhite xor clRed);
  if ShowPoits then
  begin
    Canvas.Pen.Width := 2;
    for i := 2 to length(points) - 1 do
      Canvas.Ellipse(WorldToScrn(Points[i]).x - 5,
        WorldToScrn(Points[i]).y - 5,
        WorldToScrn(Points[i]).x + 5,
        WorldToScrn(Points[i]).y + 5);
  end;
  Canvas.Pen.Mode := pmCopy;
end;

procedure TLine.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 2 - min(W, 50),
    WorldToScrn(min(points[0], points[1])).y - 2 - min(W, 50),
    WorldToScrn(max(points[0], points[1])).x + 2 + min(W, 50),
    WorldToScrn(max(points[0], points[1])).y + 2 + min(W, 50));
  Canvas.Pen.Color := clWhite;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 3 - min(W, 50),
    WorldToScrn(min(points[0], points[1])).y - 3 - min(W, 50),
    WorldToScrn(max(points[0], points[1])).x + 3 + min(W, 50),
    WorldToScrn(max(points[0], points[1])).y + 3 + min(W, 50));
  Canvas.Pen.Color := clRed;
  Canvas.Pen.Mode := pmCopy;
  if ShowPoits then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Ellipse(WorldToScrn(Points[0]).x - 5,
      WorldToScrn(Points[0]).y - 5,
      WorldToScrn(Points[0]).x + 5,
      WorldToScrn(Points[0]).y + 5);
    Canvas.Ellipse(WorldToScrn(Points[1]).x - 5,
      WorldToScrn(Points[1]).y - 5,
      WorldToScrn(Points[1]).x + 5,
      WorldToScrn(Points[1]).y + 5);
  end;
  Canvas.Pen.Mode := pmCopy;
end;

procedure TRectZoom.DrawOutLine(Canvas: TCanvas);
begin
end;

{ PointInFigure }

function TPolyline.PointInFigure(point: TFloatPoint): boolean;
var
  i: integer;
begin
  for i := 0 to length(points) - 2 do
  begin
    Result :=
      ((IsPointOnLine(points[i], points[i + 1], point, min(W, 45))) or
      (IsPointInEllipse(points[i], point, min(W, 50), min(W, 50) + 5)) or
      (IsPointInEllipse(points[i + 1], point, min(W, 50), min(W, 50) + 5)));
    if Result then
      break;
  end;
end;

function TLine.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := ((IsPointOnLine(points[0], points[1], point, min(W, 45))) or
    (IsPointInEllipse(points[0], point, min(W, 50), min(W, 50) + 5)) or
    (IsPointInEllipse(points[1], point, min(W, 50), min(W, 50) + 5)));
end;

function TRectangle.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointInRect(min(points[0], points[1]), max(points[0], points[1]), point);
end;

function TEllipse.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointInEllipse((max(points[1], points[0]) + min(points[1], points[0])) /
    2, point, abs((max(points[1], points[0]) - min(points[1], points[0])).x / 2),
    abs((max(points[1], points[0]) - min(points[1], points[0])).y / 2));
end;

function TRoundRect.PointInFigure(point: TFloatPoint): boolean;
var
  ryy, rxx: real;
begin
  ryy := RX / 2;
  rxx := RY / 2;
  if ((abs(points[0].x - points[1].x)) > (rxx * 2)) and
    ((abs(points[0].y - points[1].y)) > (ryy * 2)) then
  begin
    Result := IsPointInRect((min(points[0], points[1]) + FloatPoint(rxx + 1, 0)),
      (max(points[0], points[1]) - FloatPoint(rxx + 1, 0)), point) or
      (IsPointInRect((min(points[0], points[1]) + FloatPoint(0, ryy + 1)),
      (max(points[0], points[1]) - FloatPoint(0, ryy + 1)), point)) or
      (IsPointInEllipse((min(points[0], points[1]) + FloatPoint(rxx, ryy)),
      point, rxx - 1, ryy - 1)) or (IsPointInEllipse(
      (max(points[0], points[1]) - FloatPoint(rxx, ryy)), point, rxx - 1, ryy - 1)) or
      (IsPointInEllipse((FloatPoint(min(points[0], points[1]).x,
      max(points[0], points[1]).y) + FloatPoint(rxx, -ryy)), point, rxx - 1, ryy - 1)) or
      (IspointInEllipse((FloatPoint(max(points[0], points[1]).x,
      min(points[0], points[1]).y) + FloatPoint(-rxx, ryy)), point, rxx - 1, ryy - 1));
  end;
  if ((abs(points[0].x - points[1].x)) < (rxx * 2)) and
    ((abs(points[0].y - points[1].y)) < (ryy * 2)) then
  begin
    Result := IsPointInEllipse((max(points[1], points[0]) + min(points[1], points[0])) /
      2, point, abs((max(points[1], points[0]) - min(points[1], points[0])).x / 2),
      abs((max(points[1], points[0]) - min(points[1], points[0])).y / 2));
  end;
  if ((abs(points[0].x - points[1].x)) > (rxx * 2)) and
    ((abs(points[0].y - points[1].y)) < (ryy * 2)) then
  begin
    Result := IsPointInRect((min(points[0], points[1]) + FloatPoint(rxx, 0)),
      (max(points[0], points[1]) - FloatPoint(rxx, 0)), point) or
      (IsPointInEllipse((min(points[0], points[1]) + floatpoint(
      rxx, abs(points[1].y - points[0].y) / 2)), point, rxx,
      abs(points[0].y - points[1].y) / 2)) or
      (IsPointInEllipse(max(points[0], points[1]) + floatpoint(
      -rxx, -(abs(points[1].y - points[0].y) / 2)), point, rxx,
      abs(points[0].y - points[1].y) / 2));
  end;
  if ((abs(points[0].x - points[1].x)) < (rxx * 2)) and
    ((abs(points[0].y - points[1].y)) > (ryy * 2)) then
  begin
    Result := IsPointInRect((min(points[0], points[1]) + FloatPoint(0, ryy)),
      (max(points[0], points[1]) - FloatPoint(0, ryy)), point) or
      (IsPointInEllipse((min(points[0], points[1]) + floatpoint(
      abs(points[1].x - points[0].x) / 2, ryy)), point,
      abs(points[1].x - points[0].x) / 2, ryy)) or
      (IsPointInEllipse((max(points[0], points[1]) -
      floatpoint(abs(points[1].x - points[0].x) / 2, ryy)), point,
      abs(points[1].x - points[0].x) / 2, ryy));
  end;
end;

function TRectZoom.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := False;
end;

{ FigureInRect }

function TPolyline.FigureInRect(point1, point2: TFloatPoint): boolean;
begin
  Result := isrectinrect(max(point1, point2), min(point1, point2),
    max(points[0], points[1]), min(points[0], points[1]));
end;

function TLine.FigureInRect(point1, point2: TFloatPoint): boolean;
begin
  Result := isrectinrect(max(point1, point2), min(point1, point2),
    max(points[0], points[1]), min(points[0], points[1]));
end;

function TRectangle.FigureInRect(point1, point2: TFloatPoint): boolean;
begin
  Result := isrectinrect(max(point1, point2), min(point1, point2),
    max(points[0], points[1]), min(points[0], points[1]));
end;

function TEllipse.FigureInRect(point1, point2: TFloatPoint): boolean;
begin
  Result := isrectinrect(max(point1, point2), min(point1, point2),
    max(points[0], points[1]), min(points[0], points[1]));
end;

function TRoundRect.FigureInRect(point1, point2: TFloatPoint): boolean;
begin
  Result := isrectinrect(max(point1, point2), min(point1, point2),
    max(points[0], points[1]), min(points[0], points[1]));
end;

function TRectZoom.FigureInRect(point1, point2: TFloatPoint): boolean;
begin
  Result := False;
end;

{ CheckPoint }

function TPolyline.CheckPoint(point: TFloatPoint): PFloatPoint;
var
  i: integer;
begin
  Result := nil;
  for i := 2 to Length(Points) - 1 do
  begin
    if IsPointInEllipse(points[i], point, 5, 5) then
      Result := @points[i];
  end;
end;

function TLine.CheckPoint(point: TFloatPoint): PFloatPoint;
begin
  Result := nil;
  if IsPointInEllipse(points[0], point, 5, 5) then
    Result := @points[0];
  if IsPointInEllipse(points[1], point, 5, 5) then
    Result := @points[1];
end;

function TRectangle.CheckPoint(point: TFloatPoint): PFloatPoint;
var
  p1, p2: TFloatPoint;
begin
  Result := nil;
  if IsPointInEllipse(min(points[0], points[1]), point, 5, 5) then
  begin
    p1 := min(points[0], points[1]);
    p2 := max(points[0], points[1]);
    points[0] := p1;
    points[1] := p2;
    Result := @points[0];
  end;
  if IsPointInEllipse(max(points[0], points[1]), point, 5, 5) then
  begin
    p1 := min(points[0], points[1]);
    p2 := max(points[0], points[1]);
    points[0] := p1;
    points[1] := p2;
    Result := @points[1];
  end;
  if IsPointInEllipse(floatpoint(min(points[0], points[1]).x,
    max(points[0], points[1]).y), point, 5, 5) then
  begin
    p1 := floatpoint(min(points[0], points[1]).x, max(points[0], points[1]).y);
    p2 := floatpoint(max(points[0], points[1]).x, min(points[0], points[1]).y);
    points[0] := p1;
    points[1] := p2;
    Result := @points[0];
  end;
  if IsPointInEllipse(floatpoint(max(points[0], points[1]).x,
    min(points[0], points[1]).y), point, 5, 5) then
  begin
    p1 := floatpoint(min(points[0], points[1]).x, max(points[0], points[1]).y);
    p2 := floatpoint(max(points[0], points[1]).x, min(points[0], points[1]).y);
    points[0] := p1;
    points[1] := p2;
    Result := @points[1];
  end;
end;

function TEllipse.CheckPoint(point: TFloatPoint): PFloatPoint;
var
  p1, p2: TFloatPoint;
begin
  Result := nil;
  if IsPointInEllipse(min(points[0], points[1]), point, 5, 5) then
  begin
    p1 := min(points[0], points[1]);
    p2 := max(points[0], points[1]);
    points[0] := p1;
    points[1] := p2;
    Result := @points[0];
  end;
  if IsPointInEllipse(max(points[0], points[1]), point, 5, 5) then
  begin
    p1 := min(points[0], points[1]);
    p2 := max(points[0], points[1]);
    points[0] := p1;
    points[1] := p2;
    Result := @points[1];
  end;
  if IsPointInEllipse(floatpoint(min(points[0], points[1]).x,
    max(points[0], points[1]).y), point, 5, 5) then
  begin
    p1 := floatpoint(min(points[0], points[1]).x, max(points[0], points[1]).y);
    p2 := floatpoint(max(points[0], points[1]).x, min(points[0], points[1]).y);
    points[0] := p1;
    points[1] := p2;
    Result := @points[0];
  end;
  if IsPointInEllipse(floatpoint(max(points[0], points[1]).x,
    min(points[0], points[1]).y), point, 5, 5) then
  begin
    p1 := floatpoint(min(points[0], points[1]).x, max(points[0], points[1]).y);
    p2 := floatpoint(max(points[0], points[1]).x, min(points[0], points[1]).y);
    points[0] := p1;
    points[1] := p2;
    Result := @points[1];
  end;
end;

function TRoundRect.CheckPoint(point: TFloatPoint): PFloatPoint;
var
  p1, p2: TFloatPoint;
begin
  Result := nil;
  if IsPointInEllipse(min(points[0], points[1]), point, 5, 5) then
  begin
    p1 := min(points[0], points[1]);
    p2 := max(points[0], points[1]);
    points[0] := p1;
    points[1] := p2;
    Result := @points[0];
  end;
  if IsPointInEllipse(max(points[0], points[1]), point, 5, 5) then
  begin
    p1 := min(points[0], points[1]);
    p2 := max(points[0], points[1]);
    points[0] := p1;
    points[1] := p2;
    Result := @points[1];
  end;
  if IsPointInEllipse(floatpoint(min(points[0], points[1]).x,
    max(points[0], points[1]).y), point, 5, 5) then
  begin
    p1 := floatpoint(min(points[0], points[1]).x, max(points[0], points[1]).y);
    p2 := floatpoint(max(points[0], points[1]).x, min(points[0], points[1]).y);
    points[0] := p1;
    points[1] := p2;
    Result := @points[0];
  end;
  if IsPointInEllipse(floatpoint(max(points[0], points[1]).x,
    min(points[0], points[1]).y), point, 5, 5) then
  begin
    p1 := floatpoint(min(points[0], points[1]).x, max(points[0], points[1]).y);
    p2 := floatpoint(max(points[0], points[1]).x, min(points[0], points[1]).y);
    points[0] := p1;
    points[1] := p2;
    Result := @points[1];
  end;
end;

function TRectZoom.CheckPoint(point: TFloatPoint): PFloatPoint;
begin
  Result := nil;
end;

{ Create }

constructor TPolyline.Create(point: TFloatPoint; Wd: integer; PStyle: TPenStyle);
begin
  SetLength(Points, 3);
  Points[0] := Point;
  Points[1] := Point;
  Points[2] := Point;
  W := min(wd, 50);
  PS := PStyle;
  PC := ColorPen;
end;

constructor TLine.Create(point: TFloatPoint; Wd: integer; PStyle: TPenStyle);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  W := min(Wd, 50);
  PS := PStyle;
  PC := ColorPen;
end;

constructor TRectangle.Create(point: TFloatPoint; Wd: integer;
  PStyle: TPenStyle; BStyle: TBrushStyle);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  W := Wd;
  PS := PStyle;
  BS := BStyle;
  PC := ColorPen;
  BC := ColorBrush;
end;

constructor TEllipse.Create(point: TFloatPoint; Wd: integer; PStyle: TPenStyle;
  BStyle: TBrushStyle);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  W := Wd;
  PS := PStyle;
  PC := ColorPen;
  BC := ColorBrush;
end;

constructor TRectZoom.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
end;

constructor TRoundRect.Create(point: TFloatPoint; Wd, RadX, RadY: integer;
  PStyle: TPenStyle; BStyle: TBrushStyle);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  W := Wd;
  PS := PStyle;
  BS := BStyle;
  RY := RadX;
  RX := RadY;
  PC := ColorPen;
  BC := ColorBrush;
end;

procedure TLine.SetW(i:integer);
begin
  w:=min(i,50);
end;

procedure TpolyLine.SetW(i:integer);
begin
  w:=min(i,50);
end;

end.
