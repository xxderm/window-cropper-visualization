unit Contour;

interface

uses
  System.Types, System.SysUtils, System.Variants, System.Classes;

type
  TRectF = record
    x1, y1, x2, y2: double;
  end;

  TPointI = record
    x, y: Integer;
  end;

  TLine = record
    startPoint, endPoint: TPointI;
  end;

  IContourPoint = interface
    function getX: double;
    function getY: double;
  end;

  IContourBit = interface
    function getPointCount: integer;
    function isClosed: Boolean;
    function getPoint(const idx: integer): IContourPoint;
  end;

  IContour = interface
    function getContourBitCount: integer;
    function getContourBit(const idx: integer): IContourBit;
  end;

  IContours = interface
    function getContourCount: integer;
    function getContour(const idx: integer): IContour;
  end;

  IContourEdit = interface(IContour)
    procedure AddContourBit(const bit: IContourBit);
  end;

  IContourBitEdit = interface(IContourBit)
    procedure AddPoint(const x, y, value: double);
    procedure SetClosed(const closed: boolean);
  end;

  TContourPoint = class(TInterfacedObject, IContourPoint)
  public
    constructor Create(x, y: double);
  private
    Fx, Fy: double;
  protected
    function getX: double;
    function getY: double;
  end;

  TContours = class(TInterfacedObject, IContours)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FItems: TInterfaceList;
  public
    function getContourCount: integer;
    function getContour(const idx: integer): IContour;
    procedure addContour(const contour: IContour);
  end;

  TContourBitEdit2 = class(TInterfacedObject, IContourBit, IContourBitEdit)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FItems: TInterfaceList;
    FClosed: boolean;
  public //IContourBit
    function getPointCount: integer;
    function isClosed: boolean;
    function getPoint(const idx: integer): IContourPoint;
  public //IContourBitEdit
    procedure addPoint(const x, y, value: double);
    procedure setClosed(const closed: boolean);
  end;

  TContourEdit2 = class(TInterfacedObject, IContour, IContourEdit)
  public
    constructor Create;
    destructor Destroy; override;
  private
    FItems: TInterfaceList;
  public //IContour
    function getContourBitCount: integer;
    function getContourBit(const idx: integer): IContourBit;
  public //IContourEdit
    procedure addContourBit(const bit: IContourBit);
  end;

  function CutContoursByWindow(const contours: IContours; const window: TRectF): IContour;

  function PointInRect(const point: TPointI; const rect: TRectF): Boolean;

  function LineIntersectsRectangle(const line: TLine; const rect: TRectF): Boolean;

implementation

{ LineIntersectsRectangle }

function LineIntersectsRectangle(const line: TLine; const rect: TRectF): Boolean;
var
  lineLeft, lineRight, lineTop, lineBottom: Boolean;
begin
  lineLeft := (line.startPoint.x <= rect.x2) and (line.endPoint.x >= rect.x1);
  lineRight := (line.startPoint.x >= rect.x1) and (line.endPoint.x <= rect.x2);
  lineTop := (line.startPoint.y <= rect.y2) and (line.endPoint.y >= rect.y1);
  lineBottom := (line.startPoint.y >= rect.y1) and (line.endPoint.y <= rect.y2);

  result := (lineLeft or lineRight) and (lineTop or lineBottom);
end;

{ PointInRect }

function PointInRect(const point: TPointI; const rect: TRectF): Boolean;
begin
  result := (point.X >= rect.x1) and (point.X <= rect.x2) and
            (point.Y >= rect.y1) and (point.Y <= rect.y2);
end;

{ CutContoursByWindow }

function CutContoursByWindow(const contours: IContours; const window: TRectF): IContour;
var
  contIdx: Integer;
  i: Integer;
  point: TPointI;
  nextPoint: TPointI;
  checkLine: TLine;
  contourBitCount: Integer;
  closedFlag: Boolean;
begin
  var insideContour: IContourEdit := TContourEdit2.Create;
  for contIdx := 0 to contours.getContourCount - 1 do
  begin
    for i := 0 to contours.getContour(contIdx).getContourBitCount	- 1 do
    begin
      // Количество фрагментов и замкнутость текущего фрагмента
      contourBitCount := contours.getContour(contIdx).getContourBitCount	- 1;
      closedFlag := contours.getContour(contIdx).getContourBit(i).isClosed;

      // Текущая точка
      point.x := round(contours.getContour(contIdx).getContourBit(i).getPoint(0).getX);
      point.y := round(contours.getContour(contIdx).getContourBit(i).getPoint(0).getY);

      // Соединяющая точка
      // Если соединяющая точка замыкается первой
      if (i = contourBitCount) and closedFlag then
      begin
        nextPoint.x := round(contours.getContour(contIdx).getContourBit(0).getPoint(0).getX);
        nextPoint.y := round(contours.getContour(contIdx).getContourBit(0).getPoint(0).getY);
      end
      else
      begin
        // Если текущая точка последняя, пропустить
        if (i = contourBitCount) then Continue;
        nextPoint.x := round(contours.getContour(contIdx).getContourBit(i + 1).getPoint(0).getX);
        nextPoint.y := round(contours.getContour(contIdx).getContourBit(i + 1).getPoint(0).getY);
      end;

      // Если какая-либо из точек входит в прямоугольник
      if PointInRect(point, window) or PointInRect(nextPoint, window) then
      begin
        insideContour.addContourBit(contours.getContour(contIdx).getContourBit(i));
        // Если текущая точка замыкается первой
        if (i = contourBitCount) and closedFlag then
        begin
          insideContour.addContourBit(contours.getContour(contIdx).getContourBit(0));
        end
        // Иначе добавить следующую точку
        else
        begin
          insideContour.addContourBit(contours.getContour(contIdx).getContourBit(i + 1));
        end;
      end
      // Если обе точки не входят в прямоугольник,
      // проверить не пересекает ли отрезок между точками данный прямоугольник
      else
      begin
        checkLine.startPoint := point;
        checkLine.endPoint := nextPoint;
        // Если пересекает, добавить
        if LineIntersectsRectangle(checkLine, window) then
        begin
          insideContour.addContourBit(contours.getContour(contIdx).getContourBit(i));
          // Если текущая точка замыкается первой
          if (i = contourBitCount) and closedFlag then
          begin
            insideContour.addContourBit(contours.getContour(contIdx).getContourBit(0));
          end
          // Иначе добавить следующую точку
          else
          begin
            insideContour.addContourBit(contours.getContour(contIdx).getContourBit(i + 1));
          end;
        end;
      end;

    end;
  end;

  result := insideContour;
end;

{ TContourPoint }

constructor TContourPoint.Create(x: Double; y: Double);//конструктор класса точки
begin
  Fx := x;
  Fy := y;
end;

function TContourPoint.getX: double; // получить x данной точки
begin
    result := Fx;
end;

function TContourPoint.getY: double; // получить y данной точки
begin
    result := Fy;
end;

{ TContours }

procedure TContours.addContour(const contour: IContour);
begin
    if contour = nil then
        exit;

    FItems.Add(contour);
end;

constructor TContours.Create;
begin
    inherited Create;

    FItems := TInterfaceList.Create;
end;

destructor TContours.Destroy;
begin
    FreeAndNil(FItems);

    inherited;
end;

function TContours.getContour(const idx: integer): IContour;
begin
    if (idx >= 0) and (idx < getContourCount) then
        result := FItems[idx] as TContourEdit2
    else
        result := nil
end;

function TContours.getContourCount: integer;
begin
    result := FItems.Count;
end;

{ TContourEdit }

procedure TContourEdit2.addContourBit(const bit: IContourBit);
begin
    FItems.Add(bit);
end;

constructor TContourEdit2.Create;
begin
    inherited Create;

    FItems := TInterfaceList.Create;
end;

destructor TContourEdit2.Destroy;
begin
    FreeAndNil(FItems);

    inherited;
end;

function TContourEdit2.getContourBit(const idx: integer): IContourBit;
begin
    result := IContourBit(FItems[idx]);
end;

function TContourEdit2.getContourBitCount: integer;
begin
    result := FItems.Count;
end;

{ TContourBitEdit }

procedure TContourBitEdit2.addPoint(const x, y, value: double);
var
    pt: IContourPoint;
begin
    pt := TContourPoint.Create(x, y);
    FItems.Add(pt);
end;

constructor TContourBitEdit2.Create();
begin
    inherited Create;

    FItems := TInterfaceList.Create;
end;

destructor TContourBitEdit2.Destroy;
begin
    FreeAndNil(FItems);

    inherited;
end;

function TContourBitEdit2.getPoint(const idx: integer): IContourPoint;
begin
    result := IContourPoint(FItems[idx]);
end;

function TContourBitEdit2.getPointCount: integer;
begin
    result := FItems.Count;
end;

function TContourBitEdit2.isClosed: boolean;
begin
    result := FClosed;
end;

procedure TContourBitEdit2.setClosed(const closed: boolean);
begin
    FClosed := closed;
end;

end.
