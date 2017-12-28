
unit SFigures;

{$mode objfpc}{$H+}{$TYPEINFO ON}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, Menus, ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType,
  Buttons, GraphMath, Math, Spin, FPCanvas, TypInfo, LCL, Windows, UScale,
  Laz2_DOM, laz2_XMLRead, laz2_XMLWrite,LCLProc,strutils;

type
  { Classes }
  { TFigure }
  FClass=class of TFigure;
  ManyPoints = array of TFloatPoint;

  TFigure = class
  protected
    P: ManyPoints;
    S: boolean;
    CL: TClass;
    function GetPoint(Index: integer): TFloatPoint;
    function GetMin(): TFloatPoint;
    function GetMax(): TFloatPoint;
  public
    property Selected: boolean read S write S default False;
    property Points: ManyPoints read P write P;
    property ClassOfFigure: TClass read CL write CL;
    property MinP:TFloatPoint read GetMin;
    property MaxP:TFloatPoint read GetMax;
    class procedure SaveFile(FileName: string);
    procedure SetLengthPoints(l: integer);
    class function LoadFile(FileName: string): boolean;
    class function LoadFigure(ANode: TDOMNode; AClass:FClass): boolean; virtual;
    procedure move(point: TFloatPoint); virtual;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
    procedure DrawOutLine(Canvas: TCanvas); virtual;
    function PointInFigure(point: TFloatPoint): boolean; virtual; abstract;
    function CheckPoint(point: TFloatPoint): PFloatPoint; virtual;
    function FigureInrect(point1, point2: TFloatPoint): boolean; virtual;
    function SaveFigure(ADoc: TXMLDocument): TDOMNode;
    function SaveFigureInString(): String;
    constructor Create(C:FClass);
  end;

  { TStandartFigure }

  TStandartFigure = class(TFigure)
  private
    PC: TColor;
    W: integer;
    PS: TPenStyle;
    procedure SetW(i: integer);virtual;
  published
    property Width: integer read W write setW default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    property PenColor: TColor read PC write PC default clBlack;
  public
    procedure Draw(Canvas: TCanvas); override;
  end;

  { TPolyline }

  TPolyline = class(TStandartFigure)
  private
    procedure SetW(i: integer);override;
  public
    procedure move(point: TFloatPoint); override;
    procedure Draw(Canvas: TCanvas); override;
    procedure DrawoutLine(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function CheckPoint(point: TFloatPoint): PFloatPoint; override;
  end;

  { TLine }

  TLine = class(TStandartFigure)
  private
    procedure SetW(i: integer);override;
  public
    procedure Draw(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    procedure DrawoutLine(Canvas: TCanvas); override;
  end;

  { TVolume }

  TVolume = class(TStandartFigure)
  private
    BC: TColor;
    BS: TBrushStyle;
  published
    property PenColor: TColor read PC write PC default clBlack;
    property Width: integer read W write W default 1;
    property PenStyle: TPenStyle read PS write PS default psClear;
    property BrushStyle: TBrushStyle read BS write BS default bsSolid;
    property BrushColor: TColor read BC write BC default clBlack;
  public
    procedure DrawoutLine(Canvas: TCanvas); override;
    procedure Draw(Canvas: TCanvas); override;
  end;

  { TRectangle }

  TRectangle = class(TVolume)
  public
    procedure Draw(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
  end;

  { TEllipse }

  TEllipse = class(TVolume)
  public
    procedure Draw(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
  end;

  { TRectZoom }

  TRectZoom = class(TFigure)
  public
    procedure Draw(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
    function FigureInRect(point1, point2: TFloatPoint): boolean; override;
  end;

  { TRoundRect }

  TRoundRect = class(TVolume)
  private
    RX: integer;
    RY: integer;
  published
    property RadiusX: integer read RX write RX default 1;
    property RadiusY: integer read RY write RY default 1;
  public
    procedure Draw(Canvas: TCanvas); override;
    function PointInFigure(point: TFloatPoint): boolean; override;
  end;

function XMLToFigures(Doc: TXMLDocument): boolean;
procedure ClipBoardToFigures(S:string);
function FiguresToXML(): TXMLDocument;
function FiguresToString(b:boolean): AnsiString;
var      { Var }
  Figures: array of TFigure;
  Drawing: boolean = False;
  SelectedNumber: integer = 0;
  IsShowPoits: boolean = False;
  CtrlButtonState: boolean = False;
  ClassesFigures: array of FClass;

implementation

{ Porocedures }
procedure TFigure.SetLengthPoints(l: integer);
begin
  SetLength(P, l);
end;

procedure AddFigure(AFigure: FClass);
begin
  SetLength(ClassesFigures, Length(ClassesFigures) + 1);
  ClassesFigures[High(ClassesFigures)] := AFigure;
end;

constructor TFigure.Create(C:FClass);
begin
   CL:=c;
end;

{ Drow }
procedure TStandartFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color := PC;
  Canvas.Pen.Width := W;
  Canvas.Pen.Style := PS;
end;

procedure TVolume.Draw(Canvas: TCanvas);
begin
  inherited;
  Canvas.Pen.Width := min(W, min(
    round(abs(p[0].x - p[1].x)*zoom/100) div 2 ,
    round(abs(p[0].y - p[1].y)*zoom/100) div 2 ));
  Canvas.Brush.Color := BC;
  Canvas.Brush.Style := BS;
end;

procedure TRoundRect.Draw(Canvas: TCanvas);
var
  WTSMaxP, WTSMinP: TPoint;
begin
  inherited;
  WTSMaxP:=WorldToScrn(MinP);
  WTSMinP:=WorldToScrn(maxp);
  Canvas.RoundRect(
    WTSMinP.x + (Canvas.Pen.Width div 2),
    WTSMinP.y + (Canvas.Pen.Width div 2),
    WTSMaxP.x - ((Canvas.Pen.Width-1) div 2),
    WTSMaxP.y - ((Canvas.Pen.Width-1) div 2),
    round((RY - (Canvas.Pen.Width div 2)) * zoom / 100),
    round((RX - (Canvas.Pen.Width div 2)) * zoom / 100));
end;

procedure TRectangle.Draw(Canvas: TCanvas);
var
  WTSMaxP, WTSMinP: TPoint;
begin
  inherited;
  WTSMaxP:=WorldToScrn(MinP);
  WTSMinP:=WorldToScrn(maxp);
  Canvas.Rectangle(
    WTSMinP.x + (Canvas.Pen.Width div 2),
    WTSMinP.y + (Canvas.Pen.Width div 2),
    WTSMaxP.x - ((Canvas.Pen.Width-1) div 2),
    WTSMaxP.y - ((Canvas.Pen.Width-1) div 2));
end;

procedure TEllipse.Draw(Canvas: TCanvas);
var
  WTSMaxP, WTSMinP: TPoint;
begin
  inherited;
  WTSMaxP:=WorldToScrn(MinP);
  WTSMinP:=WorldToScrn(maxp);
  Canvas.Ellipse(
    WTSMinP.x + (Canvas.Pen.Width div 2),
    WTSMinP.y + (Canvas.Pen.Width div 2),
    WTSMaxP.x - ((Canvas.Pen.Width-1) div 2),
    WTSMaxP.y - ((Canvas.Pen.Width-1) div 2));
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var
  i: integer;
begin
  inherited;
  for i := 2 to length(P) - 2 do
  begin
    Canvas.Line(WorldToScrn(P[i]), WorldToScrn(P[i + 1]));
  end;
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  inherited;
  Canvas.Line(WorldToScrn(P[0]), WorldToScrn(P[1]));
end;

procedure TRectZoom.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Width := 1;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psDashDot;
  Canvas.Rectangle(
    WorldToScrn(P[0]).x,
    WorldToScrn(P[0]).y,
    WorldToScrn(P[1]).x,
    WorldToScrn(P[1]).y);
end;

{ Move }

procedure TFigure.move(point: TFloatPoint);
begin
  P[0] := P[0] + point;
  P[1] := P[1] + point;
end;

procedure TPolyline.move(point: TFloatPoint);
var
  i: integer;
begin
  P[0] := FloatPoint(INFINITE, INFINITE);
  P[1] := FloatPoint(-INFINITE, -INFINITE);
  for i := 2 to Length(P) - 1 do
  begin
    P[i] := P[i] + point;
    P[0] := min(P[0], P[i]);
    P[1] := max(P[1], P[i]);
  end;
end;

{ DrawOutLine }

procedure Rect(p1,p2: Tpoint;w:integer; canvas: TCanvas);
begin
  Canvas.Rectangle(p1.x - w, p1.y - w, p2.x + w, p2.y + w);
end;

procedure Cirlce(p: TPoint; w:integer; canvas: TCanvas);
begin
  Canvas.Ellipse(p.x - w, p.y - w, p.x + w, p.y + w);
end;

procedure TFigure.DrawoutLine(Canvas: TCanvas);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := (clWhite xor clRed);
  Canvas.Pen.Width := 1;
  Canvas.Pen.Mode := pmXor;
end;


procedure TVolume.DrawOutLine(Canvas: TCanvas);
var
  a, b: TPoint;
begin
  a := WorldToScrn(minp);
  b := WorldToScrn(maxp);
  inherited;
  if IsShowPoits then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Pen.Style := psSolid;
    Cirlce(a,5,Canvas);
    Cirlce(round(floatpoint(a.x , b.y)),5,Canvas);
    Cirlce(round(floatpoint(b.x , a.y)),5,Canvas);
    Cirlce(b,5,Canvas);
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Color :=clred;
    Cirlce(a,7,Canvas);
    Cirlce(round(floatpoint(a.x , b.y)),7,Canvas);
    Cirlce(round(floatpoint(b.x , a.y)),7,Canvas);
    Cirlce(b,7,Canvas);
  end
  else
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psDash;
    Rect(a,b,1,Canvas);
    Canvas.Pen.Color :=clWhite;
    Rect(a,b,2,Canvas);
  end;
  Canvas.Pen.Mode := pmCopy;
end;


procedure TPolyline.DrawoutLine(Canvas: TCanvas);
var
  i: integer;
  a, b: TPoint;
begin
  inherited;
  if IsShowPoits then
  begin
    Canvas.Pen.Style := psSolid;
    Canvas.Pen.Width := 2;
    for i := 2 to length(P) - 1 do
    begin
      Cirlce(WorldToScrn(p[i]),5,Canvas);
      Canvas.Pen.Color :=clred;
      Canvas.Pen.Mode := pmCopy;
      Cirlce(WorldToScrn(p[i]),7,Canvas);
       Canvas.Pen.Mode := pmXor;
       Canvas.Pen.Color := (clWhite xor clRed);
    end;
  end
  else
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psDash;
    a := WorldToScrn(minp);
    b := WorldToScrn(maxp);
    Rect(a,b,2 + W div 2,Canvas);
    Canvas.Pen.Color :=clWhite;
    Rect(a,b,3 + W div 2,Canvas);
  end;
  Canvas.Pen.Mode := pmCopy;
end;

procedure TLine.DrawOutLine(Canvas: TCanvas);
var
  a, b: TPoint;
begin
  inherited;
  a := WorldToScrn(p[0]);
  b := WorldToScrn(p[1]);
  if IsShowPoits then
  begin
    Canvas.Pen.Width := 2;
    Canvas.Pen.Style := psSolid;
    Cirlce(a,5,Canvas);
    Cirlce(b,5,Canvas);
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Color :=clred;
    Cirlce(a,7,Canvas);
    Cirlce(b,7,Canvas);
  end
  else
  begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psDash;
    Rect(a,b,1,Canvas);
    Canvas.Pen.Color :=clWhite;
    Rect(a,b,2,Canvas);
  end;
  Canvas.Pen.Mode := pmCopy;
end;

{ PointInFigure }

function TPolyline.PointInFigure(point: TFloatPoint): boolean;
var
  i: integer;
begin
  for i := 0 to length(P) - 2 do
  begin
    Result := IsPointOnLine(P[i], P[i + 1], point, min(W, 45));
    if Result then
      exit;
  end;
end;

function TLine.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointOnLine(P[0], P[1], point, min(W, 45));
end;

function TRectangle.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointInRect(MinP, MaxP, point);
end;

function TEllipse.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := IsPointInEllipse((MaxP + MinP) / 2, point,
  abs((MaxP - MinP).x / 2), abs((MaxP - MinP).y / 2));
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
    Result :=
      IsPointInRect(Minp + FloatPoint(rxx + 1, 0), Maxp - FloatPoint(rxx + 1, 0), point) or
      IsPointInRect(Minp + FloatPoint(0, ryy + 1), Maxp - FloatPoint(0, ryy + 1), point) or
      IsPointInEllipse(Minp + FloatPoint(rxx, ryy), point, rxx - 1, ryy - 1) or
      IsPointInEllipse(Maxp - FloatPoint(rxx, ryy), point, rxx - 1, ryy - 1) or
      IsPointInEllipse(Minp + FloatPoint(rxx, -ryy), point, rxx - 1, ryy - 1) or
      IspointInEllipse(Maxp + FloatPoint(-rxx, ryy), point, rxx - 1, ryy - 1);
  end;
  if ((abs(P[0].x - P[1].x)) < (rxx * 2)) and
    ((abs(P[0].y - P[1].y)) < (ryy * 2)) then
  begin
    Result := IsPointInEllipse((Maxp + Minp) / 2, point,
    abs((Maxp - Minp).x / 2), abs((Maxp - Minp).y / 2));
  end;
  if ((abs(P[0].x - P[1].x)) > (rxx * 2)) and
    ((abs(P[0].y - P[1].y)) < (ryy * 2)) then
  begin
    Result :=
      IsPointInRect(Minp + FloatPoint(rxx, 0),Maxp - FloatPoint(rxx, 0), point) or
      IsPointInEllipse(Minp + floatpoint(rxx, abs(P[1].y - P[0].y) / 2), point, rxx, abs(P[0].y - P[1].y) / 2) or
      IsPointInEllipse(Maxp + floatpoint(-rxx, -abs(P[1].y - P[0].y) / 2), point, rxx, abs(P[0].y - P[1].y) / 2);
  end;
  if ((abs(P[0].x - P[1].x)) < (rxx * 2)) and
    ((abs(P[0].y - P[1].y)) > (ryy * 2)) then
  begin
    Result :=
      IsPointInRect(Minp + FloatPoint(0, ryy), Maxp - FloatPoint(0, ryy), point) or
      IsPointInEllipse(Minp + floatpoint(abs(P[1].x - P[0].x) / 2, ryy), point, abs(P[1].x - P[0].x) / 2, ryy) or
      IsPointInEllipse(Maxp - floatpoint(abs(P[1].x - P[0].x) / 2, ryy), point, abs(P[1].x - P[0].x) / 2, ryy);
  end;
end;

function TRectZoom.PointInFigure(point: TFloatPoint): boolean;
begin
  Result := False;
end;

{ FigureInRect }

function TFigure.FigureInrect(point1, point2: TFloatPoint): boolean;
begin
  Result := IsRectInRect(max(point1, point2), min(point1, point2),
    maxp, minp);
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

function TFigure.CheckPoint(point: TFloatPoint): PFloatPoint;
var
  p1, p2: TFloatPoint;
begin
  Result := nil;
  if IsPointInEllipse(minp, point, 5 / zoom * 100 + 1, 5 /
    zoom * 100 + 1) then
  begin
    p1 := minp;
    p2 := maxp;
    P[0] := p1;
    P[1] := p2;
    Result := @P[0];
  end;
  if IsPointInEllipse(maxp, point, 5 / zoom * 100 + 1, 5 /
    zoom * 100 + 1) then
  begin
    p1 := minp;
    p2 := maxp;
    P[0] := p1;
    P[1] := p2;
    Result := @P[1];
  end;
  if IsPointInEllipse(floatpoint(minp.x, maxp.y),
    point, 5 / zoom * 100 + 1, 5 / zoom * 100 + 1) then
  begin
    p1 := floatpoint(minp.x, maxp.y);
    p2 := floatpoint(maxp.x, minp.y);
    P[0] := p1;
    P[1] := p2;
    Result := @P[0];
  end;
  if IsPointInEllipse(floatpoint(MaxP.x, MinP.y),
    point, 5 / zoom * 100 + 1, 5 / zoom * 100 + 1) then
  begin
    p1 := floatpoint(minp.x, maxp.y);
    p2 := floatpoint(maxp.x, minp.y);
    P[0] := p1;
    P[1] := p2;
    Result := @P[1];
  end;
end;

procedure TStandartFigure.SetW(i: integer);
begin
  w := min(i, 50);
end;

procedure TLine.SetW(i: integer);
begin
  w := min(i, 50);
end;

procedure TPolyline.SetW(i: integer);
begin
  w := min(i, 50);
end;

function TFigure.GetPoint(Index: integer): TFloatPoint;
begin
  Result := P[Index];
end;

function TFigure.GetMin(): TFloatPoint;
begin
  Result:=min(p[0],p[1]);
end;

function TFigure.GetMax(): TFloatPoint;
begin
  Result:=max(p[0],p[1]);
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
  TDOMElement(FiguresNode).SetAttribute('Offset.x', inttostr(Offset.x));
  TDOMElement(FiguresNode).SetAttribute('Offset.y', inttostr(Offset.y));
  TDOMElement(FiguresNode).SetAttribute('zoom', inttostr(zoom));
  Doc.AppendChild(FiguresNode);
  FiguresNode := Doc.DocumentElement;
  for i := 0 to High(Figures) do
      FiguresNode.AppendChild(Figures[i].SaveFigure(Doc));
  Result := Doc;
end;

function Tfigure.SaveFigure(ADoc: TXMLDocument): TDOMNode;
var
  PNode: TDOMNode;
var
  i, n: integer;
  pp: PPropList;
begin
  Result := ADoc.CreateElement(Self.ClassName);
  n:=GetPropList(self,pp);
  for i:=0 to n-1 do
  begin
    TDOMElement(Result).SetAttribute(pp^[i]^.Name, GetPropValue(self,pp^[i]));
  end;
  for i := 0 to length(Points) - 1 do
  begin
    PNode := ADoc.CreateElement('point');
    TDOMElement(PNode).SetAttribute('x', FloatToStr(Points[i].X));
    TDOMElement(PNode).SetAttribute('y', FloatToStr(Points[i].Y));
    Result.AppendChild(PNode);
  end;
end;

function FiguresToString(b:boolean): String;
var
  i: integer;
begin
  result:='<'+ 'Figures'+' Offset.x="'+inttostr(Offset.x)+'" Offset.y="'+inttostr(Offset.y)+'" zoom="'+IntToStr(zoom)+'"'+'>'+#13;
  for i := 0 to High(Figures) do
   if Figures[i].Selected or b then
      Result:=Result+Figures[i].SaveFigureInString();
  Result:=result+'</Figures>';
end;

function Tfigure.SaveFigureInString(): String;
var
  PNode: TDOMNode;
var
  i, n: integer;
  pp: PPropList;
begin
  Result :=#32+#32+'<'+Self.ClassName;
  n:=GetPropList(self,pp);
  for i:=0 to n-1 do
  begin
    result:=Result+' '+pp^[i]^.Name+'="'+ String(GetPropValue(self,pp^[i]))+'"';
  end;
  result:=Result+'>'+#13;
  for i := 0 to length(Points) - 1 do
  begin
    result:=Result+'    '+'<point x="'+FloatToStr(Points[i].X)+'" y="'+FloatToStr(Points[i].Y)+'"/>'+#13;
  end;
  result:=result+'  </'+Self.ClassName+'>'+#13;
  FreeMemAndNil(pp);
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
    if Doc.DocumentElement.Attributes.Length=3 then
    begin
      Offset.x:=StrToInt(Doc.DocumentElement.Attributes.Item[0].NodeValue);
      Offset.y:=StrToInt(Doc.DocumentElement.Attributes.Item[1].NodeValue);
      zoom:=StrToInt(Doc.DocumentElement.Attributes.Item[2].NodeValue);
    end
    else
    begin
      Offset.x:=0;
      Offset.y:=0;
      zoom:=100;
    end;
    FigNode := Doc.DocumentElement.FirstChild;
    while FigNode <> nil do
    begin
      for i := 0 to High(ClassesFigures) do
        if FigNode.NodeName = ClassesFigures[i].ClassName then
          if not ClassesFigures[i].LoadFigure(FigNode,ClassesFigures[i]) then
          begin
            exit(False);
          end;
      FigNode := FigNode.GetNextNodeSkipChildren;
    end;

  except
    exit(False);
  end;
end;


procedure ClipBoardToFigures(s:string);
var
  t: TStringStream;
  Doc: TXMLDocument;
  FigNode: TDOMNode;
  i: integer;
  b: boolean;
  l:integer;
begin
  t := TStringStream.Create(s);
  t.Position:=0;
  l:=length(Figures);
  ReadXMLFile(Doc, t);
  if Doc.DocumentElement.NodeName <> 'Figures' then exit;
    FigNode := Doc.DocumentElement.FirstChild;
    while FigNode <> nil do
    begin
      for i := 0 to High(ClassesFigures) do
        if FigNode.NodeName = ClassesFigures[i].ClassName then
          if not ClassesFigures[i].LoadFigure(FigNode,ClassesFigures[i]) then
          begin
            setlength(Figures,l);
            exit;
          end;
      FigNode := FigNode.GetNextNodeSkipChildren;
    end;
end;

class function TFigure.LoadFigure(ANode: TDOMNode; AClass:FClass): boolean;
var
  F: TFigure;
  i: integer;
  PNode: TDOMNode;
begin
  try
    SetLength(Figures, Length(Figures) + 1);
    F := AClass.Create(AClass);
    for i := 0 to ANode.Attributes.Length - 1 do
       if IsPublishedProp(AClass, ANode.Attributes.Item[i].NodeName) then
         SetPropValue(F, ANode.Attributes.Item[i].NodeName, ANode.Attributes.Item[i].NodeValue);
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
    SetLength(Figures, Length(Figures)-1);
    exit(False);
  end;
end;

initialization

  AddFigure(TPolyline);
  AddFigure(Tline);
  AddFigure(TRectangle);
  AddFigure(TEllipse);
  AddFigure(TRoundRect);
end.
