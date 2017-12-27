unit SParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Grids, LCLIntf, LCLType, Buttons, GraphMath, Math, Spin,
  FPCanvas, TypInfo, LCL, SFigures, UScale;

{ TProperty }
type
  TProperty = class
    Res: variant;
    Name: string;
    procedure OnChange(Sender: TObject); virtual;
    destructor Destroy; virtual; abstract;
  end;

  ArrayOfProperty = array of TProperty;

  TSpinProperty = class(TProperty)
    SpinLabel: TLabel;
    SpinEdit: TSpinEdit;
    procedure OnChange(Sender: TObject);
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
  end;

  TColorProperty = class(TProperty)
    Button: TColorButton;
    procedure OnChange(Sender: TObject);
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
  end;

  TPenStyleProperty = class(TProperty)
    PenStylesLabel: TLabel;
    PenStylesBox: TComboBox;
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
    procedure OnChange(Sender: TObject);
    procedure PenStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStylesLable: TLabel;
    BrushStylesBox: TComboBox;
    constructor Create(s: string; n: integer; Panel: TPanel);
    destructor Destroy; override; overload;
    procedure OnChange(Sender: TObject);
    procedure BrushStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;
 procedure MakeParams(cl: Tclass; var AP: ArrayOfProperty);
 procedure MakeSelectParams(var AP: ArrayOfProperty);

 var  PropertyPanel: TPanel;
   ColorPanelTool: TPanel;
implementation

{ Procedures }

procedure MakeParams(cl: Tclass; var AP: ArrayOfProperty);
var
  n, i: integer;
  p: PPropList;
  s: string;
begin
  for i := 0 to high(ap) do
    ap[i].Destroy;
  n := GetPropList(cl, p);
  SetLength(AP, n);
  for i := 0 to n - 1 do
  begin
    s := p^[i]^.PropType^.Name;
    case s of
      'TGraphicsColor':
      begin
        Ap[i] := TColorProperty.Create(p^[i]^.Name, i, ColorPanelTool);
      end;
      'LongInt':
      begin
        Ap[i] := TSpinProperty.Create(p^[i]^.Name, i, PropertyPanel);
      end;
      'TFPPenStyle':
      begin
        Ap[i] := TPenStyleProperty.Create(p^[i]^.Name, i, PropertyPanel);
      end;
      'TFPBrushStyle':
      begin
        Ap[i] := TBrushStyleProperty.Create(p^[i]^.Name, i, PropertyPanel);
      end;
    end;
  end;
end;

procedure MakeSelectParams(var AP: ArrayOfProperty);
var
  ParamNameList, ParamTypeList: array of string;
  f1: boolean;
  i, j, k, n: integer;
  p: PPropList;
begin
  for i := 0 to high(ap) do
    ap[i].Destroy;
  for i := 0 to length(Figures) - 1 do
  begin
    if Figures[i].Selected then
    begin
      n := GetPropList(Figures[i], p);
      f1 := True;
      for j := 0 to n - 1 do
      begin
        f1 := True;
        for k := 0 to length(ParamTypeList) - 1 do
        begin
          if ((p^[j]^.PropType^.Name = ParamTypeList[k]) and
            (p^[j]^.Name = ParamnameList[k])) then
            f1 := False;
        end;
        if f1 then
        begin
          SetLength(ParamTypeList, Length(ParamTypeList) + 1);
          ParamTypeList[high(ParamTypeList)] := p^[j]^.PropType^.Name;
          SetLength(ParamnameList, Length(ParamnameList) + 1);
          ParamnameList[high(ParamnameList)] := p^[j]^.Name;
        end;
      end;
    end;
  end;
  SetLength(AP, LENGTH(ParamTypeList));
  for i := 0 to length(ParamTypeList) - 1 do
  begin
    case ParamTypeList[i] of
      'TGraphicsColor':
      begin
        Ap[i] := TColorProperty.Create(ParamNameList[i], i, ColorPanelTool);
      end;
      'LongInt':
      begin
        Ap[i] := TSpinProperty.Create(ParamNameList[i], i + 2, PropertyPanel);
      end;
      'TFPPenStyle':
      begin
        Ap[i] := TPenStyleProperty.Create(ParamNameList[i], i + 2,
          PropertyPanel);
      end;
      'TFPBrushStyle':
      begin
        Ap[i] := TBrushStyleProperty.Create(ParamNameList[i],
          i + 2, PropertyPanel);
      end;
    end;
  end;
  PropertyPanel.Height := 35 * Length(ParamNameList);
end;

procedure TProperty.OnChange(Sender: TObject);
var
  i: TFigure;
begin
  if SelectedNumber > 0 then
  begin
    for i in Figures do
      if i.Selected then
        if IsPublishedProp(i, Name) then
          SetPropValue(i, Name, res);
  end;
  InvalidateHandler;
end;

procedure TSpinProperty.OnChange(Sender: TObject);
begin
  Res := (Sender as TSpinEdit).Value;
  inherited;
end;

procedure TColorProperty.OnChange(Sender: TObject);
begin
  Res := (Sender as TColorButton).ButtonColor;
  inherited;
end;

procedure TBrushStyleProperty.OnChange(Sender: TObject);
begin
  Res := CaseBrushStyle((Sender as TComboBox).ItemIndex);
  inherited;
end;

procedure TPenStyleProperty.OnChange(Sender: TObject);
begin
  Res := CasePenStyle((Sender as TComboBox).ItemIndex);
  inherited;
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

{ PRPCreateDestroy }

constructor TSpinProperty.Create(s: string; n: integer; Panel: Tpanel);
begin
  SpinLabel := TLabel.Create(Panel);
  SpinLabel.Caption := s;
  SpinLabel.Align := alTop;
  SpinLabel.Top := n * 100;
  SpinLabel.Parent := Panel;
  SpinEdit := TSpinEdit.Create(Panel);
  SpinEdit.Align := alTop;
  SpinEdit.Parent := Panel;
  SpinEdit.MinValue := 1;
  SpinEdit.MaxValue := 10000;
  SpinEdit.Value := 1;
  SpinEdit.Width := 130;
  SpinEdit.Top := n * 100 + 50;
  SpinEdit.Alignment := taLeftJustify;
  SpinEdit.OnChange := @OnChange;
  res := SpinEdit.Value;
  Name := s;
end;

constructor TColorProperty.Create(s: string; n: integer; Panel: Tpanel);
begin
  Button := TColorButton.Create(Panel);
  Button.Align := alTop;
  Button.Parent := Panel;
  Button.Width := 32;
  Button.Height := 32;
  Button.Top := n * 100 + 50;
  Button.OnColorChanged := @OnChange;
  res := Button.ButtonColor;
  Name := s;
end;

constructor TPenStyleProperty.Create(s: string; n: integer; Panel: TPanel);
begin
  PenStylesLabel := TLabel.Create(Panel);
  PenStylesLabel.Top := 100 * n;
  PenStylesLabel.Caption := s;
  PenStyleslabel.Align := alTop;
  PenStylesLabel.Parent := Panel;
  PenStylesBox := TComboBox.Create(Panel);
  PenStylesBox.Align := alTop;
  PenStylesBox.ReadOnly := True;
  PenStylesBox.Top := 100 * n + 50;
  PenStylesBox.Items.CommaText := ',,,,';
  PenStylesBox.Style := csOwnerDrawFixed;
  PenStylesBox.ItemIndex := 0;
  PenStylesBox.Width := 130;
  PenStylesBox.Parent := Panel;
  PenStylesBox.OnDrawItem := @PenStylesBoxDrawItem;
  PenStylesBox.OnChange := @OnChange;
  Res := CasePenStyle(0);
  Name := s;
end;

constructor TBrushStyleProperty.Create(s: string; n: integer; Panel: TPanel);
begin
  BrushStylesLable := TLabel.Create(Panel);
  BrushStylesLable.Top := 100 * n;
  BrushStylesLable.Caption := s;
  BrushStylesLable.Align := alTop;
  BrushStylesLable.Parent := Panel;
  BrushStylesBox := TComboBox.Create(Panel);
  BrushStylesBox.Items.CommaText := ',,,,,,,';
  BrushStylesBox.ReadOnly := True;
  BrushStylesBox.Top := 100 * n + 50;
  BrushStylesBox.Parent := Panel;
  BrushStylesBox.Width := 130;
  BrushStylesBox.Style := csOwnerDrawFixed;
  BrushStylesBox.ItemIndex := 0;
  BrushStylesBox.Align := alTop;
  BrushStylesBox.OnDrawItem := @BrushStylesBoxDrawItem;
  BrushStylesBox.OnChange := @OnChange;
  Res := CaseBrushStyle(0);
  Name := s;
end;

destructor TSpinProperty.Destroy;
begin
  FreeAndNil(SpinEdit);
  FreeAndNil(SpinLabel);
end;

destructor TPenStyleProperty.Destroy;
begin
  FreeAndNil(PenStylesBox);
  FreeAndNil(PenStylesLabel);
end;

destructor TBrushStyleProperty.Destroy;
begin
  FreeAndNil(BrushStylesBox);
  FreeAndNil(BrushStylesLable);
end;

destructor TColorProperty.Destroy;
begin
  FreeAndNil(Button);
end;

end.
