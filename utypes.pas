unit uTypes;

{$mode objfpc}{$H+}

interface

type
  TProductInfo = record
    Found: Boolean;
    Code: string;
    Name: string;
    Price: Double;

    Boom: Boolean;
    QtyMeth: Boolean;
    QtyA: Double;
    PriceA: Double;
    QtyB: Double;
    PriceB: Double;
  end;

  TTransactionRecord = record
    ID: Integer;
    TransactionNo: string;
    TransactionDate: string;
    SubTotal: Double;
    PaymentStatus: string;
    Discount: Double;
    Total: Double;
    PayAmount: Double;
    ChangeAmount: Double;
    PaymentType: string;
    PaymentProvider: string;
    CardLast4: string;
    TransactionType: string;
    ReturnFromTransactionID: Integer;
  end;

  TReturnItem = record
    ItemID: Integer;
    ProductCode: string;
    ProductName: string;
    Price: Double;
    OriginalQty: Double;
    ReturnQty: Double;
    DiscountValue: Double;
    DiscountType: string;
    TotalPrice: Double;
  end;

implementation

end.
