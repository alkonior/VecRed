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
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure DrawoutLine(Canvas: TCanvas); virtual; abstract;
    function PointInFigure(point: TFloatPoint): boolean; virtual; abstract;
    function FigureInrect(point1, point2: TFloatPoint): boolean; virtual; abstract;
  end;

  TPolyline = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    constructor Create(point: TFloatPoint; Width: integer; PStyle: TPenStyle);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
  end;

  TLine = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    constructor Create(point: TFloatPoint; Width: integer; PStyle: TPenStyle);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
  end;

  TRectangle = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    C2: TColor;
    B: TBrushStyle;
    constructor Create(point: TFloatPoint; Width: integer; PStyle: TPenStyle;
      BStyle: TBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
  end;

  TEllipse = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    C2: TColor;
    B: TBrushStyle;
    constructor Create(point: TFloatPoint; Width: integer; PStyle: TPenStyle;
      BStyle: TBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
  end;

  TRectZoom = class(TFigure)
    constructor Create(point: TFloatPoint);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
  end;

  TRoundRect = class(TFigure)
    C1: TColor;
    W: integer;
    P: TPenStyle;
    C2: TColor;
    B: TBrushStyle;
    RY, RX: integer;
    constructor Create(point: TFloatPoint; Width, RadX, RadY: integer;
      PStyle: TPenStyle; BStyle: TBrushStyle);
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
  end;

var      { Var }
  Figures: array of TFigure;
  Drawing: boolean = False;
  SelectedNumber: integer = 0;
  ColorPen: TColor = clBlack;
  ColorBrush: TColor = clWhite;

implementation

{ Porocedures }
{ Drow }

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(points[0]).x -
    WorldToScrn(points[1]).x) div 2 + 1, abs(WorldToScrn(points[0]).y -
    WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x + (Canvas.Pen.Width div 2),
    WorldToScrn(min(points[0], points[1])).y + (Canvas.Pen.Width div 2),
    WorldToScrn(max(points[0], points[1])).x - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(points[0], points[1])).y - (Canvas.Pen.Width div
    2 - ((Canvas.Pen.Width + 1) mod 2)),
    round((RX - (Canvas.Pen.Width div 2)) * zoom / 100),
    round((RY - (Canvas.Pen.Width div 2)) * zoom / 100));
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(points[0]).x -
    WorldToScrn(points[1]).x) div 2 + 1, abs(WorldToScrn(points[0]).y -
    WorldToScrn(points[1]).y) div 2 + 1));
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
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
  Canvas.Pen.Color := C1;
  Canvas.Brush.Color := C2;
  Canvas.Pen.Style := P;
  Canvas.Brush.Style := B;
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
  Canvas.Pen.Color := C1;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := P;
  for i := 2 to length(points) - 2 do
  begin
    Canvas.Line(WorldToScrn(Points[i]), WorldToScrn(points[i + 1]));
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := C1;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := P;
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
    round((RX + (Wid)) * zoom / 100),
    round((RY + (Wid)) * zoom / 100));
  Canvas.Pen.Color := clWhite;
  Canvas.RoundRect(WorldToScrn(min(points[0], points[1])).x - 2,
    WorldToScrn(min(points[0], points[1])).y - 2,
    WorldToScrn(max(points[0], points[1])).x + 2,
    WorldToScrn(max(points[0], points[1])).y + 2,
    round((RX + Wid) * zoom / 100),
    round((RY + Wid) * zoom / 100));
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
  Canvas.Pen.Mode := pmCopy;
end;

procedure TPolyline.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 2 - min(w, 50),
    WorldToScrn(min(points[0], points[1])).y - 2 - min(w, 50),
    WorldToScrn(max(points[0], points[1])).x + 2 + min(w, 50),
    WorldToScrn(max(points[0], points[1])).y + 2 + min(w, 50));
  Canvas.Pen.Color := clWhite;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 3 - min(w, 50),
    WorldToScrn(min(points[0], points[1])).y - 3 - min(w, 50),
    WorldToScrn(max(points[0], points[1])).x + 3 + min(w, 50),
    WorldToScrn(max(points[0], points[1])).y + 3 + min(w, 50));
  Canvas.Pen.Mode := pmCopy;
end;

procedure TLine.DrawOutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 2;
  Canvas.Pen.Style := psDash;
  Canvas.Pen.Mode := pmXor;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 2 - min(w, 50),
    WorldToScrn(min(points[0], points[1])).y - 2 - min(w, 50),
    WorldToScrn(max(points[0], points[1])).x + 2 + min(w, 50),
    WorldToScrn(max(points[0], points[1])).y + 2 + min(w, 50));
  Canvas.Pen.Color := clWhite;
  Canvas.rectangle(WorldToScrn(min(points[0], points[1])).x - 3 - min(w, 50),
    WorldToScrn(min(points[0], points[1])).y - 3 - min(w, 50),
    WorldToScrn(max(points[0], points[1])).x + 3 + min(w, 50),
    WorldToScrn(max(points[0], points[1])).y + 3 + min(w, 50));
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
      ((IsPointOnLine(points[i], points[i + 1], point, min(w, 45))) or
      (IsPointInEllipse(points[i], point, min(w, 50), min(w, 50))) or
      (IsPointInEllipse(points[i + 1], point, min(w, 50), min(w, 50))));
    if Result then
      break;
  end;
end;

function TLine.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := ((IsPointOnLine(points[0], points[1], point, min(w, 45))) or
    (IsPointInEllipse(points[0], point, min(w, 50), min(w, 50))) or
    (IsPointInEllipse(points[1], point, min(w, 50), min(w, 50))));
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
  ryy := ry / 2;
  rxx := rx / 2;
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

{ Create }

constructor TPolyline.Create(point: TFloatPoint; Width: integer; PStyle: TPenStyle);
begin
  SetLength(Points, 3);
  Points[0] := Point;
  Points[1] := Point;
  Points[2] := Point;
  W := Width;
  P := PStyle;
  C1 := ColorPen;
end;

constructor TLine.Create(point: TFloatPoint; Width: integer; PStyle: TPenStyle);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  W := Width;
  P := PStyle;
  c1 := ColorPen;
end;

constructor TRectangle.Create(point: TFloatPoint; Width: integer;
  PStyle: TPenStyle; BStyle: TBrushStyle);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  W := Width;
  P := PStyle;
  b := BStyle;
  c1 := ColorPen;
  c2 := ColorBrush;
end;

constructor TEllipse.Create(point: TFloatPoint; Width: integer;
  PStyle: TPenStyle; BStyle: TBrushStyle);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  W := Width;
  P := PStyle;
  c1 := ColorPen;
  c2 := ColorBrush;
end;

constructor TRectZoom.Create(point: TFloatPoint);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
end;

constructor TRoundRect.Create(point: TFloatPoint; Width, RadX, RadY: integer;
  PStyle: TPenStyle; BStyle: TBrushStyle);
begin
  SetLength(Points, 2);
  Points[0] := Point;
  Points[1] := Point;
  W := Width;
  P := PStyle;
  b := BStyle;
  rx := RadX;
  ry := RadY;
  c1 := ColorPen;
  c2 := ColorBrush;
end;

end.
