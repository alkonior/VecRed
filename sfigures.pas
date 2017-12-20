
unit SFigures;

{$mode objfpc}{$H+}{$TYPEINFO ON}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType,
  Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows, UScale,
  Laz2_DOM, laz2_XMLRead, laz2_XMLWrite;

type
  { Classes }
  { TFigure }
  ManyPoints = array of TFloatPoint;

  TFigure = class
  protected
    P: ManyPoints;
    S: boolean;
    CL: TClass;
    function GetPoint(Index: integer): TFloatPoint;
    procedure Setpoint(Index: integer; Value: TFloatPoint);
  public
    property Selected: boolean read S write S default False;
    property Points: ManyPoints read P write P;
    property ClassOfFigure: TClass read CL write CL;
    class procedure SaveFile(FileName: string);
    procedure SetLengthPoints(l: integer);
    class function LoadFile(FileName: string): boolean;
    class function LoadFigure(ANode: TDOMNode): boolean; virtual; abstract;
    procedure move(point: TFloatPoint); virtual; abstract;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure DrawoutLine(Canvas: TCanvas); virtual; abstract;
    function PointInFigure(point: TFloatPoint): boolean; virtual; abstract;
    function CheckPoint(point: TFloatPoint): PFloatPoint; virtual;
    function FigureInrect(point1, point2: TFloatPoint): boolean; virtual;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; virtual; abstract;

  end;

  TPolyline = class(TFigure)
  private
    PC: TColor;
    W: integer;
    PS: TPenStyle;
    procedure SetW(i: integer);
  published
    property PenColor: TColor read PC write PC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
  public
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

  TLine = class(TFigure)
  private
    PC: TColor;
    W: integer;
    PS: TPenStyle;
    procedure SetW(i: integer);
  published
    property PenColor: TColor read PC write PC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
  public
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

  TRectangle = class(TFigure)
  private
    PC: TColor;
    BC: TColor;
    W: integer;
    PS: TPenStyle;
    BS: TBrushStyle;
  published
    property PenColor: TColor read PC write PC default clBlack;
    property BrushColor: TColor read BC write BC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    property BrushStyle: TBrushStyle read BS write BS default bsClear;
  public
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

  TEllipse = class(TFigure)
  private
    PC: TColor;
    W: integer;
    PS: TPenStyle;
    BS: TBrushStyle;
    BC: TColor;
  published
    property PenColor: TColor read PC write PC default clBlack;
    property BrushColor: TColor read BC write BC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    property BrushStyle: TBrushStyle read BS write BS default bsClear;
  public
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

  TRectZoom = class(TFigure)
  public
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
  end;

  TRoundRect = class(TFigure)
  private
    PC: TColor;
    BC: TColor;
    W: integer;
    PS: TPenStyle;
    BS: TBrushStyle;
    RX: integer;
    RY: integer;
  published
    property PenColor: TColor read PC write PC default clBlack;
    property BrushColor: TColor read BC write BC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    property BrushStyle: TBrushStyle read BS write BS default bsClear;
    property RadiusX: integer read RX write RX default 1;
    property RadiusY: integer read RY write RY default 1;
  public
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode; override;
    class function LoadFigure(ANode: TDOMNode): boolean; override;
  end;

function XMLToFigures(Doc: TXMLDocument): boolean;
function FiguresToXML(): TXMLDocument;
procedure ShowPoints(P1,P2:TFloatPoint;canvas:TCanvas);

var      { Var }
  Figures: array of TFigure;
  Drawing: boolean = False;
  SelectedNumber: integer = 0;
  IsShowPoits: boolean = False;
  CtrlButtonState: boolean = False;
  ClassesFigures: array of TFigure;
  IsSaved: boolean = True;

implementation

{ Porocedures }
procedure TFigure.SetLengthPoints(l: integer);
begin
  SetLength(P, l);
end;

procedure AddFigure(AFigure: TFigure);
begin
  SetLength(ClassesFigures, Length(ClassesFigures) + 1);
  ClassesFigures[High(ClassesFigures)] := AFigure;
end;

{ Drow }

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(P[0]).x - WorldToScrn(P[1]).x) div
    2 + 1, abs(WorldToScrn(P[0]).y - WorldToScrn(P[1]).y) div 2 + 1));
  Canvas.Pen.Color := PC;
  Canvas.Brush.Color := BC;
  Canvas.Pen.Style := PS;
  Canvas.Brush.Style := BS;
  Canvas.RoundRect(WorldToScrn(min(P[0], P[1])).x + (Canvas.Pen.Width div 2),
    WorldToScrn(min(P[0], P[1])).y + (Canvas.Pen.Width div 2),
    WorldToScrn(max(P[0], P[1])).x - (Canvas.Pen.Width div 2 -
    ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(P[0], P[1])).y - (Canvas.Pen.Width div 2 -
    ((Canvas.Pen.Width + 1) mod 2)),
    round((RY - (Canvas.Pen.Width div 2)) * zoom / 100),
    round((RX - (Canvas.Pen.Width div 2)) * zoom / 100));
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(P[0]).x - WorldToScrn(P[1]).x) div
    2 + 1, abs(WorldToScrn(P[0]).y - WorldToScrn(P[1]).y) div 2 + 1));
  Canvas.Pen.Color := PC;
  Canvas.Brush.Color := BC;
  Canvas.Pen.Style := PS;
  Canvas.Brush.Style := BS;
  Canvas.Rectangle(WorldToScrn(min(P[0], P[1])).x + (Canvas.Pen.Width div 2),
    WorldToScrn(min(P[0], P[1])).y + (Canvas.Pen.Width div 2),
    WorldToScrn(max(P[0], P[1])).x - (Canvas.Pen.Width div 2 -
    ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(P[0], P[1])).y - (Canvas.Pen.Width div 2 -
    ((Canvas.Pen.Width + 1) mod 2)));
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := min(W, min(abs(WorldToScrn(P[0]).x - WorldToScrn(P[1]).x) div
    2 + 1, abs(WorldToScrn(P[0]).y - WorldToScrn(P[1]).y) div 2 + 1));
  Canvas.Pen.Color := PC;
  Canvas.Brush.Color := BC;
  Canvas.Pen.Style := PS;
  Canvas.Brush.Style := BS;
  Canvas.Ellipse(WorldToScrn(min(P[0], P[1])).x +
    (Canvas.Pen.Width div 2 - ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(min(P[0], P[1])).y + (Canvas.Pen.Width div 2 -
    ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(P[0], P[1])).x - (Canvas.Pen.Width div 2 -
    ((Canvas.Pen.Width + 1) mod 2)),
    WorldToScrn(max(P[0], P[1])).y - (Canvas.Pen.Width div 2 -
    ((Canvas.Pen.Width + 1) mod 2)));
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  i: integer;
begin
  Canvas.Pen.Color := PC;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := PS;
  for i := 2 to length(P) - 2 do
  begin
    Canvas.Line(WorldToScrn(P[i]), WorldToScrn(P[i + 1]));
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := PC;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := PS;
  Canvas.Line(WorldToScrn(P[0]), WorldToScrn(P[1]));
end;

procedure TRectZoom.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psDashDot;
  Canvas.Rectangle(WorldToScrn(P[0]).x, WorldToScrn(P[0]).y,
    WorldToScrn(P[1]).x, WorldToScrn(P[1]).y);
  Canvas.Brush.Style := bsSolid;
end;

{ move }

procedure TRoundRect.move(point: TFloatPoint);
begin
  P[0] := P[0] + point;
  P[1] := P[1] + point;
end;

procedure TRectangle.move(point: TFloatPoint);
begin
  P[0] := P[0] + point;
  P[1] := P[1] + point;
end;

procedure TEllipse.move(point: TFloatPoint);
begin
  P[0] := P[0] + point;
  P[1] := P[1] + point;
end;

procedure TPolyline.move(point: TFloatPoint);
var
  i: integer;
begin
  P[0] := FloatPoint(100000000, 10000000);
  P[1] := FloatPoint(-100000000, -10000000);
  for i := 2 to Length(P) - 1 do
  begin
    P[i] := P[i] + point;
    P[0] := min(P[0], P[i]);
    P[1] := max(P[1], P[i]);
  end;
end;

procedure TLine.move(point: TFloatPoint);
begin
  P[0] := P[0] + point;
  P[1] := P[1] + point;
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
  if IsShowPoits then
  begin
    ShowPoints(p[0],p[1],Canvas);
  end
  else
  begin
    wid := min(W, min(abs(WorldToScrn(P[0]).x - WorldToScrn(P[1]).x) div
      2 + 1, abs(WorldToScrn(P[0]).y - WorldToScrn(P[1]).y) div 2 + 1));
    Canvas.RoundRect(WorldToScrn(min(P[0], P[1])).x - 1,
      WorldToScrn(min(P[0], P[1])).y - 1,
      WorldToScrn(max(P[0], P[1])).x + 1,
      WorldToScrn(max(P[0], P[1])).y + 1,
      round((RY + (Wid)) * zoom / 100),
      round((RX + (Wid)) * zoom / 100));
    Canvas.Pen.Color := clWhite;
    Canvas.RoundRect(WorldToScrn(min(P[0], P[1])).x - 2,
      WorldToScrn(min(P[0], P[1])).y - 2,
      WorldToScrn(max(P[0], P[1])).x + 2,
      WorldToScrn(max(P[0], P[1])).y + 2,
      round((RY + Wid) * zoom / 100),
      round((RX + Wid) * zoom / 100));
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
  if IsShowPoits then
  begin
    ShowPoints(p[0],p[1],Canvas);
  end
  else
  begin
    Canvas.Rectangle(WorldToScrn(min(P[0], P[1])).x - 1,
      WorldToScrn(min(P[0], P[1])).y - 1,
      WorldToScrn(max(P[0], P[1])).x + 1,
      WorldToScrn(max(P[0], P[1])).y + 1);
    Canvas.Pen.Color := clWhite;
    Canvas.Rectangle(WorldToScrn(min(P[0], P[1])).x - 2,
      WorldToScrn(min(P[0], P[1])).y - 2,
      WorldToScrn(max(P[0], P[1])).x + 2,
      WorldToScrn(max(P[0], P[1])).y + 2);
    Canvas.Pen.Color := (clWhite xor clRed);
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
  if IsShowPoits then
  begin
    ShowPoints(p[0],p[1],Canvas);
  end
  else
  begin
    Canvas.Ellipse(WorldToScrn(min(P[0], P[1])).x - 2,
      WorldToScrn(min(P[0], P[1])).y - 2,
      WorldToScrn(max(P[0], P[1])).x + 2,
      WorldToScrn(max(P[0], P[1])).y + 2);
    Canvas.Pen.Color := clWhite;
    Canvas.Ellipse(WorldToScrn(min(P[0], P[1])).x - 4,
      WorldToScrn(min(P[0], P[1])).y - 4,
      WorldToScrn(max(P[0], P[1])).x + 4,
      WorldToScrn(max(P[0], P[1])).y + 4);
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
  if IsShowPoits then
  begin
    Canvas.Pen.Width := 2;
    for i := 2 to length(P) - 1 do
      Canvas.Ellipse(WorldToScrn(P[i]).x - 5,
        WorldToScrn(P[i]).y - 5,
        WorldToScrn(P[i]).x + 5,
        WorldToScrn(P[i]).y + 5);
  end
  else
  begin
    Canvas.rectangle(WorldToScrn(min(P[0], P[1])).x - 2 - min(W div 2, 50),
      WorldToScrn(min(P[0], P[1])).y - 2 - min(W div 2, 50),
      WorldToScrn(max(P[0], P[1])).x + 2 + min(W div 2, 50),
      WorldToScrn(max(P[0], P[1])).y + 2 + min(W div 2, 50));
    Canvas.Pen.Color := clWhite;
    Canvas.rectangle(WorldToScrn(min(P[0], P[1])).x - 3 - min(W div 2, 50),
      WorldToScrn(min(P[0], P[1])).y - 3 - min(W div 2, 50),
      WorldToScrn(max(P[0], P[1])).x + 3 + min(W div 2, 50),
      WorldToScrn(max(P[0], P[1])).y + 3 + min(W div 2, 50));
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
  if IsShowPoits then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Ellipse(WorldToScrn(P[0]).x - 5,
      WorldToScrn(P[0]).y - 5,
      WorldToScrn(P[0]).x + 5,
      WorldToScrn(P[0]).y + 5);
    Canvas.Ellipse(WorldToScrn(P[1]).x - 5,
      WorldToScrn(P[1]).y - 5,
      WorldToScrn(P[1]).x + 5,
      WorldToScrn(P[1]).y + 5);
  end
  else
  begin
    Canvas.rectangle(WorldToScrn(min(P[0], P[1])).x - 2 - min(W, 50),
      WorldToScrn(min(P[0], P[1])).y - 2 - min(W, 50),
      WorldToScrn(max(P[0], P[1])).x + 2 + min(W, 50),
      WorldToScrn(max(P[0], P[1])).y + 2 + min(W, 50));
    Canvas.Pen.Color := clWhite;
    Canvas.rectangle(WorldToScrn(min(P[0], P[1])).x - 3 - min(W, 50),
      WorldToScrn(min(P[0], P[1])).y - 3 - min(W, 50),
      WorldToScrn(max(P[0], P[1])).x + 3 + min(W, 50),
      WorldToScrn(max(P[0], P[1])).y + 3 + min(W, 50));
    Canvas.Pen.Color := clRed;
  end;
  Canvas.Pen.Mode := pmCopy;
end;

procedure ShowPoints(P1,P2:TFloatPoint;Canvas:TCanvas);
begin
  Canvas.Pen.Width := 2;
    Canvas.Ellipse(WorldToScrn(min(P2, P1)).x - 5,
      WorldToScrn(min(P2, P1)).y - 5,
      WorldToScrn(min(P2, P1)).x + 5,
      WorldToScrn(min(P2, P1)).y + 5);
    Canvas.Ellipse(WorldToScrn(max(P2, P1)).x - 5,
      WorldToScrn(max(P2, P1)).y - 5,
      WorldToScrn(max(P2, P1)).x + 5,
      WorldToScrn(max(P2, P1)).y + 5);
    Canvas.Ellipse(WorldToScrn(min(P2, P1)).x - 5,
      WorldToScrn(max(P2, P1)).y - 5,
      WorldToScrn(min(P2, P1)).x + 5,
      WorldToScrn(max(P2, P1)).y + 5);
    Canvas.Ellipse(WorldToScrn(max(P2, P1)).x - 5,
      WorldToScrn(min(P2, P1)).y - 5,
      WorldToScrn(max(P2, P1)).x + 5,
      WorldToScrn(min(P2, P1)).y + 5);
end;

procedure TRectZoom.DrawOutLine(Canvas: TCanvas);
begin
end;

{ PointInFigure }

function TPolyline.PointInFigure(point: TFloatPoint): boolean;
var
  i: integer;
begin
  for i := 0 to length(P) - 2 do
  begin
    Result :=
      ((IsPointOnLine(P[i], P[i + 1], point, min(W, 45))) or
      (IsPointInEllipse(P[i], point, min(W, 50), min(W, 50) + 5)) or
      (IsPointInEllipse(P[i + 1], point, min(W, 50), min(W, 50) + 5)));
    if Result then
      break;
  end;
end;

function TLine.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := ((IsPointOnLine(P[0], P[1], point, min(W, 45))) or
    (IsPointInEllipse(P[0], point, min(W, 50), min(W, 50) + 5)) or
    (IsPointInEllipse(P[1], point, min(W, 50), min(W, 50) + 5)));
end;

function TRectangle.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointInRect(min(P[0], P[1]), max(P[0], P[1]), point);
end;

function TEllipse.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointInEllipse((max(P[1], P[0]) + min(P[1], P[0])) /
    2, point, abs((max(P[1], P[0]) - min(P[1], P[0])).x / 2), abs(
    (max(P[1], P[0]) - min(P[1], P[0])).y / 2));
end;

function TRoundRect.PointInFigure(point: TFloatPoint): boolean;
var
  ryy, rxx: real;
begin
  ryy := RX / 2;
  rxx := RY / 2;
  if ((abs(P[0].x - P[1].x)) > (rxx * 2)) and
    ((abs(P[0].y - P[1].y)) > (ryy * 2)) then
  begin
    Result := IsPointInRect((min(P[0], P[1]) + FloatPoint(rxx + 1, 0)),
      (max(P[0], P[1]) - FloatPoint(rxx + 1, 0)), point) or
      (IsPointInRect((min(P[0], P[1]) + FloatPoint(0, ryy + 1)),
      (max(P[0], P[1]) - FloatPoint(0, ryy + 1)), point)) or
      (IsPointInEllipse((min(P[0], P[1]) + FloatPoint(rxx, ryy)),
      point, rxx - 1, ryy - 1)) or (IsPointInEllipse(
      (max(P[0], P[1]) - FloatPoint(rxx, ryy)), point, rxx - 1, ryy - 1)) or
      (IsPointInEllipse((FloatPoint(min(P[0], P[1]).x, max(P[0], P[1]).y) +
      FloatPoint(rxx, -ryy)), point, rxx - 1, ryy - 1)) or
      (IspointInEllipse((FloatPoint(max(P[0], P[1]).x, min(P[0], P[1]).y) +
      FloatPoint(-rxx, ryy)), point, rxx - 1, ryy - 1));
  end;
  if ((abs(P[0].x - P[1].x)) < (rxx * 2)) and
    ((abs(P[0].y - P[1].y)) < (ryy * 2)) then
  begin
    Result := IsPointInEllipse((max(P[1], P[0]) + min(P[1], P[0])) /
      2, point, abs((max(P[1], P[0]) - min(P[1], P[0])).x / 2), abs(
      (max(P[1], P[0]) - min(P[1], P[0])).y / 2));
  end;
  if ((abs(P[0].x - P[1].x)) > (rxx * 2)) and
    ((abs(P[0].y - P[1].y)) < (ryy * 2)) then
  begin
    Result := IsPointInRect((min(P[0], P[1]) + FloatPoint(rxx, 0)),
      (max(P[0], P[1]) - FloatPoint(rxx, 0)), point) or
      (IsPointInEllipse((min(P[0], P[1]) + floatpoint(
      rxx, abs(P[1].y - P[0].y) / 2)), point, rxx, abs(P[0].y - P[1].y) / 2)) or
      (IsPointInEllipse(max(P[0], P[1]) + floatpoint(
      -rxx, -(abs(P[1].y - P[0].y) / 2)), point, rxx, abs(P[0].y - P[1].y) / 2));
  end;
  if ((abs(P[0].x - P[1].x)) < (rxx * 2)) and
    ((abs(P[0].y - P[1].y)) > (ryy * 2)) then
  begin
    Result := IsPointInRect((min(P[0], P[1]) + FloatPoint(0, ryy)),
      (max(P[0], P[1]) - FloatPoint(0, ryy)), point) or
      (IsPointInEllipse((min(P[0], P[1]) + floatpoint(
      abs(P[1].x - P[0].x) / 2, ryy)), point, abs(P[1].x - P[0].x) / 2, ryy)) or
      (IsPointInEllipse((max(P[0], P[1]) -
      floatpoint(abs(P[1].x - P[0].x) / 2, ryy)), point,
      abs(P[1].x - P[0].x) / 2, ryy));
  end;
end;

function TRectZoom.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := False;
end;

{ FigureInRect }

function TFigure.FigureInRect(point1, point2: TFloatPoint): boolean;
begin
  Result := isrectinrect(max(point1, point2), min(point1, point2),
    max(P[0], P[1]), min(P[0], P[1]));
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
  for i := 2 to Length(P) - 1 do
  begin
    if IsPointInEllipse(P[i], point, 5 / zoom * 100 + 1, 5 / zoom * 100 + 1) then
      Result := @P[i];
  end;
end;

function TLine.CheckPoint(point: TFloatPoint): PFloatPoint;
begin
  Result := nil;
  if IsPointInEllipse(P[0], point, 5 / zoom * 100 + 1, 5 / zoom * 100 + 1) then
    Result := @P[0];
  if IsPointInEllipse(P[1], point, 5 / zoom * 100 + 1, 5 / zoom * 100 + 1) then
    Result := @P[1];
end;

function TFigure.CheckPoint(point: TFloatPoint): PFloatPoint;
var
  p1, p2: TFloatPoint;
begin
  Result := nil;
  if IsPointInEllipse(min(P[0], P[1]), point, 5 / zoom * 100 + 1, 5 /
    zoom * 100 + 1) then
  begin
    p1 := min(P[0], P[1]);
    p2 := max(P[0], P[1]);
    P[0] := p1;
    P[1] := p2;
    Result := @P[0];
  end;
  if IsPointInEllipse(max(P[0], P[1]), point, 5 / zoom * 100 + 1, 5 /
    zoom * 100 + 1) then
  begin
    p1 := min(P[0], P[1]);
    p2 := max(P[0], P[1]);
    P[0] := p1;
    P[1] := p2;
    Result := @P[1];
  end;
  if IsPointInEllipse(floatpoint(min(P[0], P[1]).x, max(P[0], P[1]).y),
    point, 5 / zoom * 100 + 1, 5 / zoom * 100 + 1) then
  begin
    p1 := floatpoint(min(P[0], P[1]).x, max(P[0], P[1]).y);
    p2 := floatpoint(max(P[0], P[1]).x, min(P[0], P[1]).y);
    P[0] := p1;
    P[1] := p2;
    Result := @P[0];
  end;
  if IsPointInEllipse(floatpoint(max(P[0], P[1]).x, min(P[0], P[1]).y),
    point, 5 / zoom * 100 + 1, 5 / zoom * 100 + 1) then
  begin
    p1 := floatpoint(min(P[0], P[1]).x, max(P[0], P[1]).y);
    p2 := floatpoint(max(P[0], P[1]).x, min(P[0], P[1]).y);
    P[0] := p1;
    P[1] := p2;
    Result := @P[1];
  end;
end;


procedure TLine.SetW(i: integer);
begin
  w := min(i, 50);
end;

procedure TpolyLine.SetW(i: integer);
begin
  w := min(i, 50);
end;

function TFigure.GetPoint(Index: integer): TFloatPoint;
begin
  Result := P[Index];
end;

procedure TFigure.Setpoint(Index: integer; Value: TFloatPoint);
begin
  P[Index] := Value;
end;

{ Save }
class procedure TFigure.SaveFile(FileName: string);
var
  Doc: TXMLDocument;
begin
  if (Copy(FileName, Length(FileName) - 3, 4) <> '.xml') then
    Exit;
  try
    Doc := FiguresToXML();
    WriteXML(Doc, FileName);
  finally
    Doc.Free;
  end;
end;

function FiguresToXML(): TXMLDocument;
var
  Doc: TXMLDocument;
  FiguresNode: TDOMNode;
  i: integer;
begin
  Doc := TXMLDocument.Create;
  FiguresNode := Doc.CreateElement('Figures');
  Doc.AppendChild(FiguresNode);
  FiguresNode := Doc.DocumentElement;
  for i := 0 to High(Figures) do
    if Figures[i].CL <> TRectZoom then
      FiguresNode.AppendChild(Figures[i].SaveFigure(Doc));
  Result := Doc;
end;

function TPolyline.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: integer;
begin
  Result := ADoc.CreateElement('TPolyline');
  TDOMElement(Result).SetAttribute('Width', IntToStr(W));
  TDOMElement(Result).SetAttribute('PenStyle', GetEnumName(TypeInfo(PS), integer(PS)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PC));
  for i := 2 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function Tline.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: integer;
begin
  Result := ADoc.CreateElement('TLine');
  TDOMElement(Result).SetAttribute('Width', IntToStr(W));
  TDOMElement(Result).SetAttribute('PenStyle', GetEnumName(TypeInfo(PS), integer(PS)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PC));
  for i := 0 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TRectangle.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: integer;
begin
  Result := ADoc.CreateElement('TRectangle');
  TDOMElement(Result).SetAttribute('Width', IntToStr(W));
  TDOMElement(Result).SetAttribute('PenStyle', GetEnumName(TypeInfo(PS), integer(PS)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PC));
  TDOMElement(Result).SetAttribute('BrushStyle',
    GetEnumName(TypeInfo(BS), integer(BS)));
  TDOMElement(Result).SetAttribute('BrushColor', IntToStr(BC));

  for i := 0 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TEllipse.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
  i: integer;
begin
  Result := ADoc.CreateElement('TEllipse');
  TDOMElement(Result).SetAttribute('Width', IntToStr(W));
  TDOMElement(Result).SetAttribute('PenStyle', GetEnumName(TypeInfo(PS), integer(PS)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PC));
  TDOMElement(Result).SetAttribute('BrushStyle',
    GetEnumName(TypeInfo(BS), integer(BS)));
  TDOMElement(Result).SetAttribute('BrushColor', IntToStr(BC));
  for i := 0 to High(Points) do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TRoundRect.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
var
  i: integer;
begin
  Result := ADoc.CreateElement('TRoundRect');
  TDOMElement(Result).SetAttribute('Width', IntToStr(W));
  TDOMElement(Result).SetAttribute('PenStyle', GetEnumName(TypeInfo(PS), integer(PS)));
  TDOMElement(Result).SetAttribute('PenColor', IntToStr(PC));
  TDOMElement(Result).SetAttribute('BrushStyle',
    GetEnumName(TypeInfo(BS), integer(BS)));
  TDOMElement(Result).SetAttribute('BrushColor', IntToStr(BC));
  TDOMElement(Result).SetAttribute('RadiusX', IntToStr(RX));
  TDOMElement(Result).SetAttribute('RadiusY', IntToStr(RY));
  for i := 0 to length(Points) - 1 do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function TrectZoom.SaveFigure(ADoc: TXMLDocument): TDOMNode;
begin
  Result := ADoc.CreateElement('');
end;

{ Load }
class function TFigure.LoadFile(FileName: string): boolean;
var
  Doc: TXMLDocument;
begin
  if (Copy(FileName, Length(FileName) - 3, 4) <> '.xml') then
    Exit(False);
  try
    ReadXMLFile(Doc, FileName);
    Result := XMLToFigures(Doc);
  finally
    Doc.Free;
  end;
end;

function XMLToFigures(Doc: TXMLDocument): boolean;
var
  FigNode: TDOMNode;
  i: integer;
  f: TFigure;
begin
  Result := True;
  if Doc.DocumentElement.NodeName <> 'Figures' then
    Exit(False);
  for f in Figures do
    f.Destroy;
  SetLength(Figures, 0);
  try
    FigNode := Doc.DocumentElement.FirstChild;
    while FigNode <> nil do
    begin
      for i := 0 to High(ClassesFigures) do
        if FigNode.NodeName = ClassesFigures[i].ClassName then
          if not ClassesFigures[i].LoadFigure(FigNode) then
          begin
            exit(False);
          end;
      FigNode := FigNode.GetNextNodeSkipChildren;
    end;

  except
    exit(False);
  end;
end;

class function TPolyline.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TPolyline;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TPolyline.Create;
    for i := 0 to ANode.Attributes.Length - 1 do
    begin
      case ANode.Attributes.Item[i].NodeName of
        'Width': SetPropValue(F, 'Width', ANode.Attributes.Item[i].NodeValue);
        'PenStyle': SetPropValue(F, 'PenStyle', ANode.Attributes.Item[i].NodeValue);
        'PenColor': SetPropValue(F, 'PenColor', ANode.Attributes.Item[i].NodeValue);
      end;
    end;
    PNode := ANode;
    f.SetLengthPoints(2);
    f.Points[0] := floatpoint(100000, 100000);
    f.Points[1] := floatpoint(-100000, -100000);
    for i := 3 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      f.SetLengthPoints(Length(f.Points) + 1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
      f.Points[0] := min(f.Points[0], f.Points[High(f.Points)]);
      f.Points[1] := max(f.Points[0], f.Points[High(f.Points)]);
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;

class function Tline.LoadFigure(ANode: TDOMNode): boolean;
var
  F: Tline;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := Tline.Create;
    for i := 0 to ANode.Attributes.Length - 1 do
    begin
      case ANode.Attributes.Item[i].NodeName of
        'Width': SetPropValue(F, 'Width', ANode.Attributes.Item[i].NodeValue);
        'PenStyle': SetPropValue(F, 'PenStyle', ANode.Attributes.Item[i].NodeValue);
        'PenColor': SetPropValue(F, 'PenColor', ANode.Attributes.Item[i].NodeValue);
      end;
    end;
    PNode := ANode;
    for i := 1 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      f.SetLengthPoints(Length(f.Points) + 1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;

class function TRectangle.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TRectangle;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TRectangle.Create;
    for i := 0 to ANode.Attributes.Length - 1 do
    begin
      case ANode.Attributes.Item[i].NodeName of
        'Width': SetPropValue(F, 'Width', ANode.Attributes.Item[i].NodeValue);
        'PenStyle': SetPropValue(F, 'PenStyle', ANode.Attributes.Item[i].NodeValue);
        'PenColor': SetPropValue(F, 'PenColor', ANode.Attributes.Item[i].NodeValue);
        'BrushColor': SetPropValue(F, 'BrushColor', ANode.Attributes.Item[i].NodeValue);
        'BrushStyle': SetPropValue(F, 'BrushStyle', ANode.Attributes.Item[i].NodeValue);
      end;
    end;
    PNode := ANode;
    for i := 1 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      f.SetLengthPoints(Length(f.Points) + 1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;

class function TEllipse.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TEllipse;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TEllipse.Create;
    for i := 0 to ANode.Attributes.Length - 1 do
    begin
      case ANode.Attributes.Item[i].NodeName of
        'Width': SetPropValue(F, 'Width', ANode.Attributes.Item[i].NodeValue);
        'PenStyle': SetPropValue(F, 'PenStyle', ANode.Attributes.Item[i].NodeValue);
        'PenColor': SetPropValue(F, 'PenColor', ANode.Attributes.Item[i].NodeValue);
        'BrushColor': SetPropValue(F, 'BrushColor', ANode.Attributes.Item[i].NodeValue);
        'BrushStyle': SetPropValue(F, 'BrushStyle', ANode.Attributes.Item[i].NodeValue);
      end;
    end;
    PNode := ANode;
    for i := 1 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      f.SetLengthPoints(Length(f.Points) + 1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;

class function TRoundRect.LoadFigure(ANode: TDOMNode): boolean;
var
  F: TRoundRect;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := TRoundRect.Create;
    for i := 0 to ANode.Attributes.Length - 1 do
    begin
      case ANode.Attributes.Item[i].NodeName of
        'Width': SetPropValue(F, 'Width', ANode.Attributes.Item[i].NodeValue);
        'PenStyle': SetPropValue(F, 'PenStyle', ANode.Attributes.Item[i].NodeValue);
        'PenColor': SetPropValue(F, 'PenColor', ANode.Attributes.Item[i].NodeValue);
        'BrushColor': SetPropValue(F, 'BrushColor', ANode.Attributes.Item[i].NodeValue);
        'BrushStyle': SetPropValue(F, 'BrushStyle', ANode.Attributes.Item[i].NodeValue);
        'RadiusX': SetPropValue(F, 'RadiusX', ANode.Attributes.Item[i].NodeValue);
        'RadiusY': SetPropValue(F, 'RadiusY', ANode.Attributes.Item[i].NodeValue);
      end;
    end;
    PNode := ANode;
    for i := 1 to ANode.GetChildCount do
    begin
      PNode := PNode.GetNextNode;
      f.SetLengthPoints(Length(f.Points) + 1);
      f.Points[High(f.Points)] :=
        FloatPoint(StrToFloat(PNode.Attributes.Item[0].NodeValue),
        StrToFloat(PNode.Attributes.Item[1].NodeValue));
    end;
    Figures[High(Figures)] := F;
    Result := True;
  except
    exit(False);
  end;
end;


initialization

  AddFigure(TPolyline.Create);
  AddFigure(Tline.Create);
  AddFigure(TRectangle.Create);
  AddFigure(TEllipse.Create);
  AddFigure(TRoundRect.Create);
end.
