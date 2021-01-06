import useSWR from "swr";
import { useRouter } from "next/router";
import React from "react";
import AnsiUp from "ansi_up";

import { StatusIcon } from "@/icon.tsx";
import { Card, BuildInfo } from "@/card.tsx";
import * as Api from "@/api.ts";

const logFormatter = new AnsiUp();
logFormatter.use_classes = true;

export default () => {
  const router = useRouter();
  const { number } = router.query;

  const refresh = { refreshInterval: 1000 };
  const base = Api.endpoint + "/build/" + number;
  const { data, error } = useSWR<Api.Job>(
    number ? base : null,
    Api.fetchJson,
    refresh
  );

  const [selectedStep, setSelectedStep] = React.useState(0);

  let currentStep: Api.Step | null = null;
  if (data && data.steps && data.steps[selectedStep]) {
    currentStep = data.steps[selectedStep];
  }

  const { data: logs } = useSWR(
    currentStep ? base + "/step/" + currentStep.name + "/logs" : null,
    Api.fetchText,
    refresh
  );

  if (error) return <div>Failed to load</div>;
  if (!data) return <div>Loading</div>;

  return (
    <main>
      <div className="mt-6">
        <Card>
          <BuildInfo job={data} />
        </Card>
      </div>

      <div className="grid grid-cols-12 gap-4 mt-6">
        <div className="col-span-4">
          {data.steps.map((step, index: number) => {
            return (
              <a
                key={step.name}
                href="#"
                onClick={() => setSelectedStep(index)}
              >
                <Card state={step.name === currentStep?.name ? "selected" : ""}>
                  <div className="flex">
                    <StatusIcon status={step.state} />
                    <div className="ml-2">{step.name}</div>
                  </div>
                </Card>
              </a>
            );
          })}
        </div>
        <section className="output col-span-8 bg-blue-900 rounded py-4">
          {logs && showLogs(logs)}
          {!logs &&
            currentStep?.state &&
            showLogs(stepWaitMsg(currentStep.state))}
        </section>
      </div>
    </main>
  );
};

const stepWaitMsg = (state: Api.State) => {
  switch (state) {
    case "queued":
    case "assigned":
    case "ready":
      return "Waiting for step to start...";
    case "running":
      return "Waiting for logs...";
    case "succeeded":
    case "failed":
    case "unexpectedstate":
      return "No log output.";
    case "skipped":
      return "Step was skipped.";
  }
};

const showLogs = (logs: string) => {
  const lines = logs.split("\n");
  return (
    <table className="output-lines">
      <tbody>
        {lines.map((line, index) => {
          return (
            <tr key={index}>
              <td className="ol-pos">{index + 1}</td>
              <td
                className="ol-html"
                dangerouslySetInnerHTML={{
                  __html: logFormatter.ansi_to_html(line),
                }}
              />
            </tr>
          );
        })}
      </tbody>
    </table>
  );
};
