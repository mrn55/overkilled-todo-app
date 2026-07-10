import http from "k6/http";
import { check, sleep } from "k6";

export const options = {
  stages: [
    { duration: "1m", target: 5 },
    { duration: "3m", target: 15 },
    { duration: "1m", target: 0 },
  ],
  thresholds: {
    http_req_failed: ["rate<0.05"],
    http_req_duration: ["p(95)<1500"],
  },
};

const baseUrl = __ENV.BASE_URL || "http://todo-dev.example.com";
const hostHeader = __ENV.HOST_HEADER;

function params() {
  return hostHeader ? { headers: { Host: hostHeader } } : {};
}

export default function () {
  const res = http.get(`${baseUrl}/todo`, params());
  check(res, {
    "GET /todo is 200": (r) => r.status === 200,
  });
  sleep(1);
}
