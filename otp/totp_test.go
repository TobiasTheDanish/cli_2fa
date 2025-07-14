package otp

import (
	"encoding/base32"
	"testing"
)

func TestGenerateTotp(t *testing.T) {
	tests := []struct {
		name   string
		key    string
		t      uint64
		step   int
		digits int
		want   string
	}{
		{
			name:   "RFC 6238 Test case 1",
			key:    base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:      59,
			step:   30,
			digits: 8,
			want:   "94287082",
		},
		{
			name:   "RFC 6238 Test case 2",
			key:    base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:      1111111109,
			step:   30,
			digits: 8,
			want:   "07081804",
		},
		{
			name:   "RFC 6238 Test case 3",
			key:    base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:      1111111111,
			step:   30,
			digits: 8,
			want:   "14050471",
		},
		{
			name:   "RFC 6238 Test case 4",
			key:    base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:      1234567890,
			step:   30,
			digits: 8,
			want:   "89005924",
		},
		{
			name:   "RFC 6238 Test case 5",
			key:    base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:      2000000000,
			step:   30,
			digits: 8,
			want:   "69279037",
		},
		{
			name:   "RFC 6238 Test case 6",
			key:    base32.StdEncoding.EncodeToString([]byte(rfcTestKey)),
			t:      20000000000,
			step:   30,
			digits: 8,
			want:   "65353130",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := generateTotp(tt.key, tt.t, tt.step, tt.digits)

			if got != tt.want {
				t.Errorf("GenerateTotp() = %v, want %v", got, tt.want)
			}
		})
	}
}
