package otp

import (
	"testing"
)

func TestGenerateHOTP(t *testing.T) {
	testKey := "3132333435363738393031323334353637383930"

	tests := []struct {
		name    string
		key     string
		counter uint64
		digits  int
		want    int
	}{
		{
			name:    "RFC 4226 test case 1",
			key:     testKey,
			counter: 0,
			digits:  6,
			want:    755224,
		},
		{
			name:    "RFC 4226 test case 2",
			key:     testKey,
			counter: 1,
			digits:  6,
			want:    287082,
		},
		{
			name:    "RFC 4226 test case 3",
			key:     testKey,
			counter: 2,
			digits:  6,
			want:    359152,
		},
		{
			name:    "RFC 4226 test case 4",
			key:     testKey,
			counter: 3,
			digits:  6,
			want:    969429,
		},
		{
			name:    "RFC 4226 test case 5",
			key:     testKey,
			counter: 4,
			digits:  6,
			want:    338314,
		},
		{
			name:    "RFC 4226 test case 6",
			key:     testKey,
			counter: 5,
			digits:  6,
			want:    254676,
		},
		{
			name:    "RFC 4226 test case 7",
			key:     testKey,
			counter: 6,
			digits:  6,
			want:    287922,
		},
		{
			name:    "RFC 4226 test case 8",
			key:     testKey,
			counter: 7,
			digits:  6,
			want:    162583,
		},
		{
			name:    "RFC 4226 test case 9",
			key:     testKey,
			counter: 8,
			digits:  6,
			want:    399871,
		},
		{
			name:    "RFC 4226 test case 10",
			key:     testKey,
			counter: 9,
			digits:  6,
			want:    520489,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := generateHOTP(tt.key, tt.counter, tt.digits)
			if got != tt.want {
				t.Errorf("GenerateHOTP() = %v, want %v", got, tt.want)
			}
		})
	}
}
