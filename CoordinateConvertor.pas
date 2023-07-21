unit CoordinateConvertor;

interface

uses
  System.Types, System.SysUtils, System.Variants, System.Classes, Contour;

type

  TCoordinateConvertor = class(TInterfacedObject)
  public
    function convert(x, y: Integer): TPointI;
  end;

var
  convertor: TCoordinateConvertor;

implementation

{ TCoordinateConvertor }

// ѕреобразует координаты от диапазона x[-57k;-20k], y[-240k;-100k] к диапазону [50;800]
function TCoordinateConvertor.convert(x: Integer; y: Integer): TPointI;
const
  sourceMinX = -57000;
  sourceMaxX = -20000;
  sourceMinY = -240000;
  sourceMaxY = -100000;
  targetMinX = 50;
  targetMaxX = 800;
  targetMinY = 50;
  targetMaxY = 800;
begin
  result.X := Round((x - sourceMinX) * (targetMaxX - targetMinX) /
    (sourceMaxX - sourceMinX) + targetMinX);
  result.Y := Round((y - sourceMinY) * (targetMaxY - targetMinY) /
    (sourceMaxY - sourceMinY) + targetMinY);
end;

end.
