unit STools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin,
  FPCanvas, TypInfo, LCL, SFigures, UScale, SPropertes;

type

  { Classes }
  { TTool }

  TTool = class
    Icon: string;
    PRP: array of boolean;
    nPRP: integer;
    procedure FigureCreate(Point: TFloatPoint); virtual; abstract;
    procedure ChangePoint(Point: TFloatPoint); virtual; abstract;
    procedure AddPoint(Point: TFloatPoint); virtual; abstract;
    procedure MouseUp(Point: TFloatPoint); virtual; abstract;
    procedure FigureEnd(); virtual; abstract;
    procedure PropertiesCreate();
  end;

  { STools }
  TPolylineTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TPenTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TLineTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TRectangleTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TRoundRectTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TEllipseTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TZoomTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TRectZoomTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TSelectTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

  TScrollTool = class(TTool)
  public
    constructor Create;
    procedure FigureCreate(Point: TFloatPoint); override;
    procedure ChangePoint(Point: TFloatPoint); override;
    procedure AddPoint(Point: TFloatPoint); override;
    procedure FigureEnd(); override;
    procedure MouseUp(Point: TFloatPoint); override;
  end;

procedure DeletePRP();

var           { Var }
  Tools: array of TTool;
  SPoint: TFloatPoint;
  ShiftB: boolean = False;

implementation

{ Porocedures }

procedure RegisterTool(Tool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := Tool;
end;

procedure DeletePRP();
var
  i: TProperty;
begin
  for i in Propertys do
    i.deletePRP();
end;

{ PropertiesCreate }

procedure TTool.PropertiesCreate();
var
  i: integer;
begin
  PropertyPanel.Height := nPRP * 40 - nPRP;
  for i := 0 to length(Propertys) - 1 do
  begin
    if PRP[i] then
      Propertys[i].showPRP();
  end;
end;


{ FigureCreate }

procedure TPolylineTool.FigureCreate(Point: TFloatPoint);
begin
  GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TPolyLine.Create(point);
  Figures[High(Figures)].GetParams();
end;

procedure TPenTool.FigureCreate(Point: TFloatPoint);
begin
  GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TPolyLine.Create(point);
  Figures[High(Figures)].GetParams();
end;

procedure TLineTool.FigureCreate(Point: TFloatPoint);
begin
  GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TLine.Create(point);
  Figures[High(Figures)].GetParams();
end;

procedure TEllipseTool.FigureCreate(Point: TFloatPoint);
begin
  GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TEllipse.Create(point);
  Figures[High(Figures)].GetParams();
end;

procedure TRectangleTool.FigureCreate(Point: TFloatPoint);
begin
  GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectangle.Create(point);
  Figures[High(Figures)].GetParams();
end;

procedure TRoundRectTool.FigureCreate(Point: TFloatPoint);
begin
  GetMaxMin(point);
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRoundRect.Create(point);
  Figures[High(Figures)].GetParams();
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
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectZoom.Create(point);
end;

procedure TSelectTool.FigureCreate(Point: TFloatPoint);
begin
  Setlength(Figures, length(figures) + 1);
  Figures[High(Figures)] := TRectZoom.Create(point);
end;

procedure TScrollTool.FigureCreate(Point: TFloatPoint);
begin
  SPoint := Point;
end;


{ ChangePoint }

procedure TPolylineTool.ChangePoint(Point: TFloatPoint);
begin
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TPenTool.ChangePoint(Point: TFloatPoint);
begin
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  LMinPoint := MinPoint;
  LMaxPoint := MaxPoint;
  with Figures[high(Figures)] do
  begin
    SetLength(points, Length(Points) + 1);
    points[high(points)] := point;
  end;
end;

procedure TLineTool.ChangePoint(Point: TFloatPoint);
begin
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
end;

procedure TRectangleTool.ChangePoint(Point: TFloatPoint);
begin
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TRoundRectTool.ChangePoint(Point: TFloatPoint);
begin
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TEllipseTool.ChangePoint(Point: TFloatPoint);
begin
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  with Figures[high(Figures)] do
  begin
    points[high(points)] := point;
  end;
end;

procedure TZoomTool.ChangePoint(Point: TFloatPoint);
begin
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
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
  LMinPoint := MinPoint;
  LMaxPoint := MaxPoint;
  with Figures[high(Figures)] do
  begin
    SetLength(points, Length(points) + 1);
    points[high(points)] := point;
  end;
end;

procedure TLineTool.AddPoint(Point: TFloatPoint);
begin
  Drawing := False;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
end;

procedure TRectangleTool.AddPoint(Point: TFloatPoint);
begin
  Drawing := False;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
end;

procedure TRoundRectTool.AddPoint(Point: TFloatPoint);
begin
  Drawing := False;
  MinPoint := Min(LMinPoint, point);
  MaxPoint := Max(LMaxPoint, point);
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
  Drawing := False;
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
  ZoomToRect(Figures[High(Figures)].Points[0], Figures[High(Figures)].Points[1]);
  FreeAndNil(Figures[High(Figures)]);
  SetLength(Figures, Length(Figures) - 1);
  Drawing := False;
end;

procedure TSelectTool.MouseUp(Point: TFloatPoint);
var
  i: TFigure;
begin
  SelectedNumber := length(Figures);
  for i in figures do
    i.Selected := True;
  FreeAndNil(Figures[High(Figures)]);
  SetLength(Figures, Length(Figures) - 1);
  Drawing := False;
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
  FreeAndNil(Figures[High(Figures)]);
  SetLength(Figures, Length(Figures) - 1);
  Drawing := False;
end;

procedure TSelectTool.FigureEnd();
begin
  FreeAndNil(Figures[High(Figures)]);
  SetLength(Figures, Length(Figures) - 1);
  Drawing := False;
end;

procedure TZoomTool.FigureEnd();
begin
end;

procedure TScrollTool.FigureEnd();
begin
end;

{ Create }

constructor TPolylineTool.Create;
begin
  Icon := 'ico/polyline.png';
  nPRP := 2;
  SetLength(PRP, length(Propertys));
  prp[0] := True;
  prp[1] := True;
end;

constructor TPenTool.Create;
begin
  Icon := 'ico/polyline.png';
  nPRP := 2;
  SetLength(PRP, length(Propertys));
  prp[0] := True;
  prp[1] := True;
end;

constructor TLineTool.Create;
begin
  Icon := 'ico/line.png';
  nPRP := 2;
  SetLength(PRP, length(Propertys));
  prp[0] := True;
  prp[1] := True;
end;

constructor TRectangleTool.Create;
begin
  Icon := 'ico/rectangle.png';
  nPRP := 3;
  SetLength(PRP, length(Propertys));
  prp[0] := True;
  prp[1] := True;
  prp[2] := True;
end;

constructor TEllipseTool.Create;
begin
  Icon := 'ico/ellipse.png';
  nPRP := 3;
  SetLength(PRP, length(Propertys));
  prp[0] := True;
  prp[1] := True;
  prp[2] := True;
end;


constructor TZoomTool.Create;
begin
  Icon := 'ico/zoom.png';
  nPRP := 0;
  SetLength(PRP, length(Propertys));
end;

constructor TScrollTool.Create;
begin
  Icon := 'ico/Scroll.png';
  nPRP := 0;
  SetLength(PRP, length(Propertys));
end;

constructor TRectZoomTool.Create;
begin
  Icon := 'ico/rectzoom.png';
  nPRP := 0;
  SetLength(PRP, length(Propertys));
end;

constructor TRoundRectTool.Create;
begin
  Icon := 'ico/RoundRectangle.png';
  nPRP := 5;
  SetLength(PRP, length(Propertys));
  prp[0] := True;
  prp[1] := True;
  prp[2] := True;
  prp[3] := True;
  prp[4] := True;
end;

constructor TSelectTool.Create;
begin
  Icon := 'ico/select.png';
  nPRP := 1;
  SetLength(PRP, length(Propertys));
  prp[5] := True;
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
