unit UI_Status ;

interface

const CEF_VO_Show_Read = 1 ;
      CEF_VO_Show_Write = 2 ;
      CEF_VO_Show_Access = 4 ; // Access other than R/W (such as commands)
      CEF_VO_Show_Online = 8 ; // Show online/offline status
      CEF_VO_Show_Ready = 16 ; // Show ready status

type TUI_Status = class
                      public // Property handlers...
                          function Get_Caption : PChar ;
                          procedure Set_Caption( Value : PChar ) ;
                          function Get_Options : longint ;
                          procedure Set_Options( Value : longint ) ;
                          function Get_item_Color( Index : longint ) : longint ;
                          procedure Set_Item_Color( Index, Value : longint ) ;
                          function Get_Low : int64 ;
                          procedure Set_Low( Value : int64 ) ;
                          function Get_High : int64 ;
                          procedure Set_High( Value : int64 ) ;
                          function Get_Persistence : longint ;
                          procedure Set_Persistence( Value : longint ) ;
                          function Get_Unit_Size : longint ;
                          procedure Set_Unit_Size( Value : longint ) ;
                          function Get_Stages : longint ;
                          procedure Set_Stages( Value : longint ) ;

                      public // UI control...
                          // Create new mini-view as a child of the passed handle (or reassign to handle if already created).  Sizes to size of handle.
                          procedure Set_Up( Handle : longint ) ;

                          // Resize mini-view to size of parent.
                          procedure Resize ;

                          // Expand mini-view to full view.
                          procedure Expand ;

                          // Collapse full view to mini-view.
                          procedure Collapse ;

                      public // Status updates...
                          procedure Read( Address, Size : int64 ) ; // Read of Size units at Address
                          procedure Write( Address, Size : int64 ) ; // Write of Size units at Address
                          procedure Set_Online( Value : boolean ) ; // Set online state
                          procedure Set_Ready( Value : boolean ) ; // Set ready state
			              procedure Control ; // Notice of a non-R/W access

                      public // Properties...
                          property Caption : PChar
                              read Get_Caption
                              write Set_Caption ;

                          // Display options (see CEF_VO_*)
                          property Options : longint
                              read Get_Options
                              write Set_Options ;

                          // Color associated with display items (Use CEF_VO_* value for item)  Can be a combination, such as: CEF_VO_Show_Read or CEF_VO_Show_Write
                          property Item_Color[ Item : longint ] : longint
                              read Get_Item_Color
                              write Set_Item_Color ;

                          // Lowest unit address used
                          property Low : int64
                              read Get_Low
                              write Set_Low ;

                          // Highest address used
                          property High : int64
                              read Get_High
                              write Set_High ;

                          // How long indicators persist after being set (in ms)
                          property Persistence : longint 
                              read Get_Persistence
                              write Set_Persistence ;

                          // Size of unit, in bytes
                          property Unit_Size : longint
                              read Get_Unit_Size
                              write Set_Unit_Size ;

                          // Number of stages to removing an indicator on the expanded view.  For instance, a value of 3 means that the indicator darkens twice before going off
                          property Stages : longint
                              read Get_Stages
                              write Set_Stages ;
                  end ; // TUI_Status

implementation

end.
