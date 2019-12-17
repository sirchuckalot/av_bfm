/*
* Avalon Slave BFM
* Copyright (C) 2019      Charley Picker <charleypicker@yahoo.com>
*               2013-2015 Olof Kindgren <olof.kindgren@gmail.com>
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
*/

module av_bfm_slave
  #(parameter dw = 32,
    parameter aw = 32,
    parameter burstw = 8,
    parameter DEBUG = 0)
   (input               av_clk_i,
    input               av_rst_i,

    input [aw-1:0]      av_address_i,
    input [dw-1:0]      av_writedata_i,
    input [dw/8-1:0]    av_byteenable_i,
    input [burstw-1:0]  av_burstcount_i,
    input               av_write_i,
    input               av_read_i,

    output reg          av_waitrequest_o,
    output reg          av_readdatavalid_o,
    output reg [1:0]    av_response_o,
    output reg [dw-1:0] av_readdata_o);

`include "av_common.v"

   localparam       Tp       = 1;

   reg              has_next = 1'b0;

   reg              op       = READ;
   reg [aw-1:0]     address;
   reg [dw-1:0]     data;
   reg [dw/8-1:0]   mask;
   reg              cycle_type;
   reg [2:0]        burst_type;
   reg [burstw-1:0] count;

   reg              err      = 0;

   wire av_cyc = av_write_i | av_read_i;

   task init;
        begin
            av_waitrequest_o   <= #Tp 1'b1;
            av_readdatavalid_o <= #Tp 1'b0;
            av_readdata_o      <= #Tp {dw{1'b0}};
            av_response_o      <= #Tp RESPONSE_OKAY;

            if(av_rst_i !== 1'b0) begin
                if(DEBUG) $display("%0d : waiting for reset release", $time);
                @(negedge av_rst_i);
                @(posedge av_clk_i);
                if(DEBUG) $display("%0d : Reset was released", $time);
             end

            //Catch start of next cycle
            if (!av_cyc)
                @(posedge av_cyc);
            @(posedge av_clk_i);

            //Make sure that av_cyc is still asserted at next clock edge to avoid glitches
            while(av_cyc !== 1'b1)
                @(posedge av_clk_i);
            if(DEBUG) $display("%0d : Got av_cyc, burstcount = %x", $time, av_burstcount_i);

            cycle_type = get_cycle_type(av_burstcount_i);
            //cycle_type = (av_burstcount_i > 0) ? BURST_CYCLE : CLASSIC_CYCLE;

            op                  = av_write_i ? WRITE : READ;
            address             = av_address_i;
            mask                = av_byteenable_i;
            count               = av_burstcount_i;

            has_next            = 1'b1;
        end
   endtask

   task error_response;
        begin
            err = 1'b1;
            next();
            err = 1'b0;
        end
   endtask

   task read_ack;
        input [dw-1:0] data_i;
        begin
            data = data_i;
            next();
        end
   endtask

   task write_ack;
        output [dw-1:0] data_o;
        begin
            if(DEBUG) $display("%0d : Write ack", $time);
            next();
            data_o = data;
        end
   endtask

   task next;
        begin
            if(DEBUG) $display("%0d : next address=0x%h, data=0x%h, op=%b", $time, address, data, op);

            // First check if we need to send error response
            if (err) begin
                if(DEBUG) $display("%0d, Error", $time);

                // We can't continue and must abort cycle request
                av_response_o <= #Tp RESPONSE_SLAVEERROR;
                av_waitrequest_o   <= #Tp 1'b1;
                av_readdatavalid_o <= #Tp 1'b0;

                // Gracefully, complete has next counter
                has_next       = 1'b0;
                count          = 0;
            end else

            // If not error, then we should check for read
            if(op === READ) begin
                av_response_o      <= #Tp RESPONSE_OKAY;
                av_readdata_o      <= #Tp data;
                av_waitrequest_o   <= #Tp 1'b0;
                av_readdatavalid_o <= #Tp 1'b1;
            end else

            // If not error or read, then we should check for write
            if (op === WRITE) begin
                // Per the Avalon spec, during bursts the master write signal indicates valid writedata
                if (cycle_type === BURST_CYCLE) begin
                    if (!av_write_i) begin
                        if(DEBUG) $display("%0d : Master stalled burst write, waiting for write signal", $time);
                        av_response_o      <= #Tp RESPONSE_OKAY;
                        av_waitrequest_o   <= #Tp 1'b1;
                        av_readdatavalid_o <= #Tp 1'b0;
                    end else begin
                        if (DEBUG) $display("%0d : Master burst write signal asserted", $time);
                        av_response_o      <= #Tp RESPONSE_OKAY;
                        av_waitrequest_o   <= #Tp 1'b0;
                        av_readdatavalid_o <= #Tp 1'b0;
                    end

                // Only write condition left is single cycle
                end else begin
                    av_response_o      <= #Tp RESPONSE_OKAY;
                    av_waitrequest_o   <= #Tp 1'b0;
                    av_readdatavalid_o <= #Tp 1'b0;
                end
            end

            // We can now release response back to master at next posedge clock
            @(posedge av_clk_i);

            // Now we can capture write data
            data = av_writedata_i;
            mask = av_byteenable_i;
            address = av_address_i;

            // Should we do it again?
            has_next = (count > 1) & !err;
            
            // Read with no error response counts down
            if ((op === READ) & !err) begin
                if (count > 0)
                    count = count - 1;
            end else
            // Write with no error response and no burst counts down
            if ( (op === WRITE) & (cycle_type === CLASSIC_CYCLE) & !err ) begin
                if (count > 0)
                    count = count - 1;
            end else
            // Write with burst, no master stall and no error counts down 
            if ( (op === WRITE) & (cycle_type === BURST_CYCLE) &  (av_write_i) & !err) begin
                if (count > 0)
                    count = count - 1;
            end

            // We always end task with normal, ready response
            av_response_o      <= #Tp RESPONSE_OKAY;
            av_waitrequest_o   <= #Tp 1'b0;
            av_readdatavalid_o <= #Tp 1'b0;
        end

   endtask

endmodule
