package otp_test

import (
	"encoding/base32"
	"testing"

	"github.com/tobiasthedanish/cli_2fa/otp"
)

var rfcTestKey = "12345678901234567890"

func TestGenerateTotp(t *testing.T) {
	tests := []struct {
		name    string
		key     string
		t       uint64
		step    int
		digits  int
		want    string
		wantErr bool
	}{
		{
			name:    "RFC 6238 Test case 1",
			key:     base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:       59,
			step:    30,
			digits:  8,
			want:    "94287082",
			wantErr: false,
		},
		{
			name:    "RFC 6238 Test case 2",
			key:     base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:       1111111109,
			step:    30,
			digits:  8,
			want:    "07081804",
			wantErr: false,
		},
		{
			name:    "RFC 6238 Test case 3",
			key:     base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:       1111111111,
			step:    30,
			digits:  8,
			want:    "14050471",
			wantErr: false,
		},
		{
			name:    "RFC 6238 Test case 4",
			key:     base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:       1234567890,
			step:    30,
			digits:  8,
			want:    "89005924",
			wantErr: false,
		},
		{
			name:    "RFC 6238 Test case 5",
			key:     base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:       2000000000,
			step:    30,
			digits:  8,
			want:    "69279037",
			wantErr: false,
		},
		{
			name:    "RFC 6238 Test case 6",
			key:     base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:       20000000000,
			step:    30,
			digits:  8,
			want:    "65353130",
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, gotErr := otp.GenerateTotp(tt.key, tt.t, tt.step, tt.digits)
			if gotErr != nil {
				if !tt.wantErr {
					t.Errorf("GenerateTotp() failed: %v", gotErr)
				}
				return
			}
			if tt.wantErr {
				t.Fatal("GenerateTotp() succeeded unexpectedly")
			}
			if got != tt.want {
				t.Errorf("GenerateTotp() = %v, want %v", got, tt.want)
			}
		})
	}
}
