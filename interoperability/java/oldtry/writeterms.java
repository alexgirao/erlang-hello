/* javac -cp .:OtpErlang-R13B01.jar writeterms.java && java -ea -cp .:OtpErlang-R13B01.jar writeterms
 */

import java.io.InputStream;
import java.io.IOException;
import java.io.FileOutputStream;

import com.ericsson.otp.erlang.*;

class writeterms {
    public static void main(String[] Args) throws java.io.FileNotFoundException, java.io.IOException, OtpErlangDecodeException {
	OtpOutputStream out = new OtpOutputStream();

	// file:write(FD, term_to_binary(1)),
	// file:write(FD, term_to_binary(1.618034)),
	// file:write(FD, term_to_binary(a_atom)),
	// file:write(FD, term_to_binary("a_string")),
	// file:write(FD, term_to_binary(true)),
	// file:write(FD, term_to_binary(false)),
	// file:write(FD, term_to_binary({a_atom, 1, 1.618034, "a_string"})),
	// file:write(FD, term_to_binary([a_list, 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144])),

	out.write_int(1);
	out.write_double(1.618034);
	out.write_atom("a_atom");
	out.write_string("a_string");
	out.write_boolean(true);
	out.write_boolean(false);

	byte[] buffer = out.toByteArray();

	FileOutputStream fout = new FileOutputStream("writeterms.bin");
	fout.write(buffer);
	fout.close();

	hexdump.dump(buffer, 0, System.out, 0, buffer.length);
    }
}
