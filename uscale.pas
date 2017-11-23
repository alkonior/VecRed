unit UScale;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, LCLIntf, LCLType, Buttons, Math,
  FPCanvas, TypInfo, Spin, graphMath;

function FloatPoint(Point: TPoint): TFloatPoint; overload;
function WorldToScrn(P: TFloatPoint): TPoint;
function ScrnToWorld(P: TPoint): TFloatPoint;
function IntToFloat(P: integer): extended;
function Round(point: TFloatPoint): TPoint; overload;
function Max(P1, P2: TFloatPoint): TFloatPoint; overload;
function Min(P1, P2: TFloatPoint): TFloatPoint; overload;
function IsPointInRect(LH, RB, P: TFloatPoint): boolean;
function IsPointInEllipse(p0, P: TFloatPoint; Rx, Ry: Float): boolean;
function IsPointOnline(P1, P2, P: TFloatPoint): boolean;
function CasePenStyle(Index: integer): TPenStyle;
function CaseBrushStyle(Index: integer): TBrushStyle;
procedure GetMaxMin(point: TFloatPoint);
procedure SetScrolBars(var Scroll1, Scroll2: TScrollBar);
procedure CenterZoom(oldzoom: double);
procedure ChangeCenter();
procedure ZoomToRect(Point1, Point2: TFloatPoint);
operator -(Addend1, Addend2: TPoint): TPoint;
operator +(Addend1, Addend2: TPoint): TPoint;
operator * (P1, P2: TFloatPoint): Float;

var
  Offset: TPoint;
  Zoom: integer = 100;
  MinPoint: TFloatPoint;
  MaxPoint: TFloatPoint;
  LMinPoint: TFloatPoint;
  LMaxPoint: TFloatPoint;
  WindowWH: TPoint;
  WindowLWH: TPoint;
  ScrollLWH: TPoint;
  LLMinPoint: TFloatPoint;
  LLMaxPoint: TFloatPoint;
  InvalidateHandler: procedure of object;

implementation

{ operator }
operator * (P1, P2: TFloatPoint): Float;
begin
  Result := abs(p1.x - p2.x) * abs(p1.y - p2.y);
end;

operator +(Addend1, Addend2: TPoint): TPoint;
begin
  Result.x := Addend1.x + Addend2.x;
  Result.y := Addend1.y + Addend2.y;
end;

operator -(Addend1, Addend2: TPoint): TPoint;
begin
  Result.x := Addend1.x - Addend2.x;
  Result.y := Addend1.y - Addend2.y;
end;

{ pointinfigure }
function IsPointInRect(LH, RB, P: TFloatPoint): boolean;
begin
  Result := (LH.x < P.x) and (LH.y < P.y) and (RB.x > P.x) and (RB.y > P.y);
end;

function IsPointInEllipse(P0, P: TFloatPoint; Rx, Ry: Float): boolean;
begin
  Result := ((((p.x - p0.x) * (p.x - p0.x)) / (rx * rx) +
    ((p.y - p0.y) * (p.y - p0.y)) / (ry * ry)) < 1);
end;

function IsPointOnLine(P1, P2, P: TFloatPoint): boolean;
var
  A, B, C, d: real;
begin
  if p2 = p1 then
  begin
    Result := False;
    exit;
  end;
  A := P1.Y - P2.y;
  B := P2.x - P1.x;
  C := P1.x * P2.Y - P2.x * P1.y;
  d := abs(a * p.x + b * p.Y + c) / sqrt(a * a + b * b);
  Result := (((p1.x > p.x) and (p2.X < p.x)) or ((p1.x < p.x) and (p2.x > p.x))) and
    (((p1.y > p.y) and (p2.y < p.y)) or ((p1.y < p.y) and (p2.y > p.y))) and (d < 10);
end;

{ Zoom }

procedure SetScrolBars(var Scroll1, Scroll2: TScrollBar);
begin
  if (minpoint.x < maxpoint.x) and (minpoint.y < maxpoint.y) then
  begin
    Scroll1.SetParams(round(offset).x,
      round(min(minpoint * zoom / 100, LMinPoint * zoom / 100)).x -
      550, round(max(maxpoint * zoom / 100, LMaxPoint * zoom / 100)).x +
      550, min(round(WindowWH.x / zoom * 100), abs(Scroll1.Min - Scroll1.Max)));
    Scroll2.SetParams(round(offset).y,
      round(min(minpoint * zoom / 100, LMinPoint * zoom / 100)).Y -
      550, round(max(maxpoint * zoom / 100, LMaxPoint * zoom / 100)).Y +
      550, min(round(WindowWH.y / zoom * 100), abs(Scroll2.Min - Scroll2.Max)));
  end
  else
  begin
    Scroll1.Min := -10000;
    Scroll1.Max := 10000;
    Scroll2.Min := -10000;
    Scroll2.Max := 10000;
    Scroll1.PageSize := 20000;
    Scroll2.PageSize := 20000;
  end;
end;

procedure GetMaxMin(point: TFloatPoint);
begin
  LLMaxPoint := MaxPoint;
  LLMinPoint := MinPoint;
  LMinPoint := MinPoint;
  LMaxPoint := MaxPoint;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  LMinPoint := MinPoint;
  LMaxPoint := MaxPoint;
end;

function Max(P1, P2: TFloatPoint): TFloatPoint;
begin
  Result.x := max(p1.x, p2.x);
  Result.y := max(p1.y, p2.y);
end;

function Min(P1, P2: TFloatPoint): TFloatPoint;
begin
  Result.x := min(p1.x, p2.x);
  Result.y := min(p1.y, p2.y);
end;

function IntToFloat(P: integer): extended;
begin
  Result := p;
end;

function round(point: TFloatPoint): TPoint;
begin
  Result.x := round(Point.x);
  Result.y := round(Point.y);
end;

function FloatPoint(Point: TPoint): TFloatPoint;
begin
  Result := Point;
end;

function WorldToScrn(P: TFloatPoint): TPoint;
begin
  Result := round(p * zoom / 100) - Offset;
end;

function ScrnToWorld(P: TPoint): TFloatPoint;
begin
  Result := (p + Offset) / zoom * 100;
end;

procedure CenterZoom(oldzoom: double);
begin
  if Zoom > oldzoom then
    Offset := Offset + round(WindowWH * (Zoom - oldzoom) / 200)
  else
    Offset := Offset - round(WindowWH * (oldZoom - zoom) / 200);

end;

procedure ChangeCenter();
begin
  Offset := Offset + round((WindowLWH - WindowWH) / 2);
end;

procedure ZoomToRect(Point1, Point2: TFloatPoint);
var
  oldz: float;
  mip, map: TFloatPoint;
begin
  oldz := zoom;
  if (point1 = Point2) then
    exit;
  Mip := min(point1, point2);
  Map := max(point1, point2);
  Point1 := map;
  point2 := mip;
  zoom := round(min(((IntToFloat(WindowWH.x) * 100) / (point1.x - point2.x)),
    (IntToFloat(WindowWH.y) * 100) / (point1.y - point2.y))) - 1;
  if Zoom > 1000 then
    Zoom := 1000;
  if (Zoom <> oldz) and (Zoom <> 100) then
  begin
    Offset := round(Point2 * Zoom / 100);
  end;
  InvalidateHandler;
end;

function CasePenStyle(Index: integer): TPenStyle;
begin
  case Index of
    0: Result := psSolid;
    1: Result := psDash;
    2: Result := psDot;
    3: Result := psDashDot;
    4: Result := psDashDotDot;
  end;
end;

function CaseBrushStyle(Index: integer): TBrushStyle;
begin
  case Index of
    0: Result := bsSolid;
    1: Result := bsBDiagonal;
    2: Result := bsDiagCross;
    3: Result := bsVertical;
    4: Result := bsCross;
    5: Result := bsFDiagonal;
    6: Result := bsHorizontal;
    7: Result := bsClear;
  end;
end;

end.
