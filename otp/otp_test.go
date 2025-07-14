package otp

import (
	"testing"
	"time"
)

func Test_otp_FromTime(t *testing.T) {
	o := New(rfcTestKey)
	tests := []struct {
		name string
		t    time.Time
		want string
	}{
		{
			name: "E2E test rfc example 1",
			t:    time.Date(1970, time.January, 1, 0, 0, 59, 0, time.UTC),
			want: "287082",
		},
		{
			name: "E2E test rfc example 1 same step",
			t:    time.Date(1970, time.January, 1, 0, 0, 39, 0, time.UTC),
			want: "287082",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := o.FromTime(tt.t)

			if got != tt.want {
				t.Errorf("FromTime() = %v, want %v", got, tt.want)
			}
		})
	}
}
