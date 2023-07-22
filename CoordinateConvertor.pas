unit CoordinateConvertor;

interface

uses
  System.Types, System.SysUtils, System.Variants, System.Classes;

type

  TPointD = record
    x, y: double;
  end;

  TCoordinateConvertor = class(TInterfacedObject)
  public
    function convert(x, y: double): TPointD;
    function convertX(x: double): double;
    function convertY(y: double): double;
  end;

const
  sourceMinX = -57000;
  sourceMaxX = -20000;
  sourceMinY = -240000;
  sourceMaxY = -100000;
  targetMinX = 50;
  targetMaxX = 800;
  targetMinY = 50;
  targetMaxY = 800;

var
  convertor: TCoordinateConvertor;

implementation

{ TCoordinateConvertor }

function TCoordinateConvertor.convertY(y: double): double;
begin
  result := Round((y - sourceMinY) * (targetMaxY - targetMinY) /
    (sourceMaxY - sourceMinY) + targetMinY);
end;

function TCoordinateConvertor.convertX(x: double): double;
begin
 result := Round((x - sourceMinX) * (targetMaxX - targetMinX) /
    (sourceMaxX - sourceMinX) + targetMinX);
end;

// ѕреобразует координаты от диапазона x[-57k;-20k], y[-240k;-100k] к диапазону [50;800]
function TCoordinateConvertor.convert(x, y: double): TPointD;
begin
  result.X := convertX(x);
  result.Y := convertY(y);
end;

end.
